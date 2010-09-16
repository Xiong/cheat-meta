use 5.008008;
use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Bundle::Cheat',
    license             => 'artistic_2',
    dist_author         => 'Xiong Changnian <xiong@cpan.org>',
    dist_version_from   => 'lib/Bundle/Cheat.pm',
    requires            => {
        'perl'                      => 5.008008,
    },
    build_requires      => {
        'Test::More'                => 0.94,
    },
    add_to_cleanup      => [ 'Bundle-Cheat-*' ],
    meta_merge              => {
        resources               => {
            repository              => 'http://github.com/Xiong/bundle-cheat',
        },
        keywords            => [ qw(
            help    usage
            docs    documentation
            cheat   sheet   cheatsheet
            summary
        ) ],
    },
);

$builder->create_build_script();
