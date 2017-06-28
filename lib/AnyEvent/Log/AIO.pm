package AnyEvent::Log::AIO;

use 5.010000;

our $VERSION = '0.01';

use AnyEvent::AIO;
use IO::AIO;
use Fcntl qw(O_RDWR O_CREAT O_APPEND);
use Time::Moment;
use Carp;

# use DDP;

use Mouse;
use strict;
no warnings 'uninitialized';

# Buffer messages, until emit discard callbacks
has 'buffer', is => 'rw', default => 1000;
has 'delay', is => 'rw', default => 0;

has 'file',   is => 'rw', required => 1;
has 'flags',  is => 'rw', required => 1, default => O_RDWR|O_CREAT|O_APPEND;
has 'mode',   is => 'rw', required => 1, default => oct('0644');
has 'retry',  is => 'rw', default => 1/3;
# # TypeConstraints is a good idea but has the ugliest error.
# # Can't allow myself to produce such shity behavior
# use Mouse::Util::TypeConstraints;
# subtype 'Callable'
# 	=> as 'Ref'
# 	=> where { warn "check $_"; UNIVERSAL::isa($_, 'CODE') || UNIVERSAL::can($_, '(&{}') };
# no Mouse::Util::TypeConstraints;
# has [qw(on_open on_close on_error)], is => 'rw', isa => 'Callable', coerce => 0;

has 'on_open', is => 'rw',
	trigger => sub { UNIVERSAL::isa($_[1], 'CODE') || UNIVERSAL::can($_[1], '(&{}') or croak "Callback required for 'on_open', got $_[1]"; };
# has 'on_close', is => 'rw',
# 	trigger => sub { UNIVERSAL::isa($_[1], 'CODE') || UNIVERSAL::can($_[1], '(&{}') or croak "Callback required for 'on_close', got $_[1]"; };
has 'on_drain', is => 'rw',
	trigger => sub { UNIVERSAL::isa($_[1], 'CODE') || UNIVERSAL::can($_[1], '(&{}') or croak "Callback required for 'on_drain', got $_[1]"; };
has 'on_error', is => 'rw',
	default => sub { warn "@_"; },
	trigger => sub { UNIVERSAL::isa($_[1], 'CODE') || UNIVERSAL::can($_[1], '(&{}') or croak "Callback required for 'on_error', got $_[1]"; };


has 'map_lf', is => 'rw', default => "\t",
	trigger => sub { $_[0]->_linegen( $_[0]->_reset_linegen() ) },
;
has 'list_sep', is => 'rw', default => " ",
	trigger => sub { $_[0]->_linegen( $_[0]->_reset_linegen() ) },
;
has 'date_format', is => 'rw',
	default => '%Y-%m-%dT%H:%M:%S%3f',
	trigger => sub { $_[0]->_linegen( $_[0]->_reset_linegen() ) },
;
has 'line_format', is => 'rw',
	default => "%d %m\n",
	trigger => sub { $_[0]->_linegen( $_[0]->_reset_linegen() ) },
;

has '_fh', is => 'rw';
has '_linegen', is => 'rw', builder => '_reset_linegen';
has '_draining', is => 'rw';
has '_rotating', is => 'rw';
has '_wq', is => 'ro', default => sub {+[]};
has '_elapsed', is => 'rw', default => 0;
has '_waited', is => 'rw', default => 0;
has '_drain_rotate', is => 'rw';

sub BUILD {
	my $self = shift;
	# p $self;
	$self->_open unless $self->delay;
}

sub _error {
	my $self = shift;
	my $msg = shift;
	if ($self->on_error) {
		$self->on_error->($self, $msg);
	}
	else {
		warn "$msg\n";
	}
	return;
}

sub _open {
	my $self = shift;
	my $cb = pop;
	delete $self->{_retry};
	aio_open $self->file, $self->flags, $self->mode, sub {
		my $fh = shift;
		my $err = "$!";
		if ($self->_fh) {
			warn "Open while have _fh";
			aio_close $self->_fh, sub {
				shift or $self->_error("Failed to close ".$self->file.": $!");
			};
		}
		if ($fh) {
			$self->_fh($fh);
			$self->on_open && $self->on_open->($self, $fh);
			$cb && $cb->(1);
			if (@{ $self->_wq }) {
				warn "Call _drain after (re)open";
				$self->_drain;
			};
		}
		else {
			$self->_error($self->file.": $err");
			$cb && $cb->(undef, "$err");
			if ($self->retry) {
				$self->{_retry} = AE::timer $self->retry, 0, sub {
					$self->_open;
				}
			}
		}
	};
	return;
}

sub open {
	my $self = shift;
	$self->_open(@_);
}

sub _close {
	my $self = shift;
	if ($self->_fh) {
		aio_close $self->_fh, sub {
			(shift >= 0) or $self->_error("Failed to close ".$self->file.": $!");
		};
	}
}

sub rotate {
	my $self = shift;
	my $cb = pop;
	$self->_rotating(1);
	my $actual_rotate = sub {
		my $fh = delete $self->{_fh};
		warn "Actual rotate $fh";
		aio_close $fh, sub {
			warn "closed: @_";
			(shift >= 0) or $self->_error("Failed to close ".$self->file.": $!");
			$self->_open(sub {
				$self->_rotating(0);
				$cb && $cb->();
			});
		};
	};
	if ($self->_draining) {
		warn "Rotate while draining";
		$self->_drain_rotate($actual_rotate);
	}
	else {
		if ($self->_fh) {
			warn "Rotate idle with fh";
			$actual_rotate->();
		}
		else {
			warn "Rotate idle without fh (reopen)";
			$self->_open(sub {
				$self->_rotating(1);
				$cb && $cb->();
			});
		}
	}
}

sub _reset_linegen {
	my $self = shift;
	my @parts = split /(%.)/s, $self->line_format;
	my @out;
	
	my $re = $self->map_lf;
	my $sep = $self->list_sep;
	my $prefix = '';
	my $msg_do = '
		my $msg = shift // "";
		my $tm = Time::Moment->now;
		if (@_) {
			if (index($msg,"%") > -1) {
				$msg = sprintf($msg, @_);
			}
			else {
				$msg = join $sep, $msg,@_;
			}
		}
	';
	if (defined $self->map_lf) {
		$msg_do .= "\t".'$msg =~ s{\n}{$re}g;'."\n";
	}

	for (@parts) {
		if (/^%d/) {
			my $date_format = $self->date_format;
			$date_format =~ s{([\\'])}{\\$1}sg;
			if ($date_format =~ /Z$/) {
				push @out, qq{Time::Moment->now_utc->strftime('$date_format')}
			}
			else {
				push @out, qq{\$tm->strftime('$date_format')}
			}
		}
		elsif (/^%m/) {
			if ($msg_do) {
				$prefix = $msg_do;
				undef $msg_do;
			}
			push @out, '$msg';
		}
		elsif (length == 0) {}
		else {
			s{([\\'])}{\\$1}sg;
			push @out, "'$_'";
		}
	}
	my $sub = 'sub { '.$prefix."\t\t".join('.',@out).", \$tm\n}";
	# say $sub;
	my $val = eval $sub or die $@;
	return $val;
}

sub log :method {
	my $self = shift;
	my $cb = pop;
	UNIVERSAL::isa( $cb, 'CODE' ) || UNIVERSAL::can($cb, '(&{}' )
		or croak "Callback required";
	my ($msg, $tm) = $self->_linegen->(@_);

	if (@{ $self->_wq } >= $self->buffer) {
		AE::postpone {
			$cb->(undef,"Buffer overflow ($self->{buffer}). Increase buffer or write slowly");
		};
		return;
	}

	push @{ $self->_wq }, [$msg,$tm,$cb];
	return if $self->_rotating;
	return unless $self->_fh;
	$self->_drain;
	return;
}

use constant {
	MSG => 0,
	TM  => 1,
	CB  => 2,

	PAGESIZE => 4096,
};

sub _drain {
	my $self = shift;
	return unless $self->_fh;
	return warn "Already draining" if $self->_draining;
	$self->_draining(1);

	my $wq = $self->_wq;

	my $next;$next = sub {
		unless ($self->_fh) {
			$self->_draining(0);
			warn "No fh for drain";
			return;
		}
		if ($self->{_drain_rotate}) {
			warn "Read after write, call _drain_rotate";
			$self->_draining(0);
			(delete $self->{_drain_rotate})->();
			return;
		}
		unless (@$wq) {
			# warn "Stop drain";
			undef $next;
			$self->_draining(0);
			$self->on_drain && $self->on_drain->($self);
			return;
		}

		my $wbuf = $wq->[0][MSG];
		my @tms  =($wq->[0][TM]);
		my @cbs  =($wq->[0][CB]);
		my $i;

		for ($i=1; $i< @$wq; $i++) {
			if (length ($wbuf) + length $wq->[$i][MSG] > PAGESIZE*2) {
				last;
			}
			$wbuf .= $wq->[$i][MSG];
			push @tms, $wq->[$i][TM];
			push @cbs, $wq->[$i][CB];
		}
		# say "consume $i from buf result in ".length $wbuf;
		splice @$wq, 0, $i;

		my $prev = Time::Moment->now;
		aio_write $self->_fh, undef,undef, $wbuf, 0, sub {
			my $now = Time::Moment->now;
			my $ela = $prev->delta_nanoseconds($now);
			# my $wait = $task->[TM]->delta_nanoseconds($now);
			$self->{_elapsed} += $ela;
			# $self->{_waited} += $wait;

			undef $wbuf; # must keep it until finish
			# say "written $i: @_ ($ela)";
			my $res = shift;

			# # Variant 1: postpone callback
			# if ($res > 0) {
			# 	AE::postpone {
			# 		$_->(1) for @cbs;
			# 	}
			# }
			# else { # -1 on error
			# 	my $err = "$!";
			# 	AE::postpone {
			# 		$_->(undef, $err) for @cbs;
			# 	}
			# }
			# goto &$next;

			# Variant 2: postpone redrain
			if ($res > 0) {
				$_->(1) for @cbs;
			}
			else { # -1 on error
				my $err = "$!";
				$_->(undef, $err) for @cbs;
			}
			goto &$next;
		};
	};$next->();
	return;
}


__PACKAGE__->meta->make_immutable();

1;
__END__

=head1 NAME

AnyEvent::Log::AIO - Write filesystem logs with IO::AIO

=head1 SYNOPSIS

    use AnyEvent::Log::AIO;
    my $logger = AnyEvent::Log::AIO->new(
        file => "/var/log/aio.log",

        # File open flags (Fcntl) and mode
        flags => O_RDWR|O_CREAT|O_APPEND, # default
        mode  => oct('0644'), # default

        # retry interval in seconds, undef to disable
        retry => 0.333, # default

        # Format of date (extended strftime, see Time::Moment)
        date_format => '%Y-%m-%dT%H:%M:%S%3f', # default

        # Format of line (%d = date, %m = message)
        line_format => "%d %m\n", # default

        # String for replacing \n in messages. undef disables feature
        map_lf   => "\t", # default

        # List separator for non-sprintf like messages (like $")
        list_sep => " ", # default

        # Internal buffer to keep messages during maintenance (rotate or inactivity)
        buffer => 1000, # default

        # When file opened
        on_open  => sub { my ($logger) = @_; },

        # When failed to open file
        on_error => sub { my ($logger, $reason) = @_; },
        
        # When write buffer became empty after writing data
        on_drain => sub { my ($logger) = @_; },
    );

    # Just after object creating file gets opened.
    # You may delay it by passing delay => 1 and then call

    $logger->open();

    # Single message could be written. Callback is mandatory (dies otherwise).
    $logger->log("some message", sub {
        my ($status, $error) = @_;
        if ($status) {
            # message written
        }
        else {
            warn "Failed to write: $error";
        }
    });

    # Or sprintf format (except cb, of course ;)
    $logger->log("some message: %s", "sprintf format", sub {
        my ($status, $error) = @_;
        if ($status) {
            # message written
        }
        else {
            warn "Failed to write: $error";
        }
    });

    # If first arg have no '%', then consider as a list of args
    $logger->log("some message: ", "common", "args", "list", sub {
        my ($status, $error) = @_;
        if ($status) {
            # message written
        }
        else {
            warn "Failed to write: $error";
        }
    });

    # Any time it is possible to call

    $logger->rotate()

    # This will cause logger to reopen log file without loosing messages.
    # (Wait for any message to finish writing, then close fd, reopen file and continue writing to new)


=head1 SEE ALSO

L<AE::log>

=head1 AUTHOR

Mons Anderson, E<lt>mons@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2017 Mons Anderson

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
