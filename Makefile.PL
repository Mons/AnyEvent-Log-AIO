use 5.010000;
use ExtUtils::MakeMaker;

our $AUTHOR = -e '.gitignore';
if ($AUTHOR) {
	system( 'pod2text lib/AnyEvent/Log/AIO.pm > README' ) and die "$!";
	#system( 'pod2markdown lib/AnyEvent/Log/AIO.pm > README.md' ) and die "$!";
	require CPAN::Meta;
	CPAN::Meta->import();
}

WriteMakefile(
	NAME              => 'AnyEvent::Log::AIO',
	VERSION_FROM      => 'lib/AnyEvent/Log/AIO.pm',
	ABSTRACT_FROM     => 'lib/AnyEvent/Log/AIO.pm',
	PREREQ_PM         => {
		'AnyEvent'      => 7,
		'AnyEvent::AIO' => 0,
		'IO::AIO'       => 0,
		'Time::Moment'  => 0,
		'Mouse'         => 2,
	},
	AUTHOR            => 'Mons Anderson <mons@cpan.org>',
	META_ADD          => { license => 'perl' },
);
