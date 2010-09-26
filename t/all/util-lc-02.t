#!/run/bin/perl
#       test-trap-01.t
#       = Copyright 2010 Xiong Changnian <xiong@cpan.org>    =
#       = Free Software = Artistic License 2.0 = NO WARRANTY =
#~ {
#~     package Acme::Teddy;
#~     sub one {1};
#~     sub dies {die'thrown'};
#~     sub prints {print'hello'};
#~ }
use 5.010001;
use strict;
use warnings;

#~ use Readonly;
#~ use feature qw(switch say state);
#~ use Perl6::Junction qw( all any none one );
#~ use List::Util qw(first max maxstr min minstr reduce shuffle sum);
#~ use File::Spec;
#~ use File::Spec::Functions qw(
#~     catdir
#~     catfile
#~     catpath
#~     splitpath
#~     curdir
#~     rootdir
#~     updir
#~     canonpath
#~ );
#~ use Cwd;

#

#~ use Acme::Teddy;

use Devel::Comments '###', '####';

my $data    = [
    [  0,  1,  2 ],
    [  0, 11, 12 ],
    [  0, 11, 22 ],
];

my @a   = @{ $data->[0] };
my @b   = @{ $data->[1] };
my @c   = @{ $data->[2] };

my @strings = ('11');

##### @a
##### @b
##### @c

use List::Compare;

my $lc  = List::Compare->new( \@a, \@b, \@c );

### $lc

my @ixs  = $lc->are_members_which(\@strings);

### @ixs

$lc->print_subset_chart;
$lc->print_equivalence_chart;

__END__

### $lc: bless( {
###               bag => [
###                        0,
###                        0,
###                        0,
###                        1,
###                        11,
###                        12,
###                        2,
###                        21,
###                        22
###                      ],
###               intersection => [
###                                 '0'
###                               ],
###               maxindex => '2',
###               nonintersection => [
###                                    '1',
###                                    '11',
###                                    '12',
###                                    '2',
###                                    '21',
###                                    '22'
###                                  ],
###               seen => {
###                         '0' => {
###                                  '0' => 1,
###                                  '1' => 1,
###                                  '2' => 1
###                                },
###                         '1' => {
###                                  '0' => 1,
###                                  '11' => 1,
###                                  '12' => 1
###                                },
###                         '2' => {
###                                  '0' => 1,
###                                  '21' => 1,
###                                  '22' => 1
###                                }
###                       },
###               shared => [
###                           '0'
###                         ],
###               symmetric_difference => [
###                                         '1',
###                                         '11',
###                                         '12',
###                                         '2',
###                                         '21',
###                                         '22'
###                                       ],
###               union => [
###                          '0',
###                          '1',
###                          '11',
###                          '12',
###                          '2',
###                          '21',
###                          '22'
###                        ],
###               xcomplement => [
###                                [
###                                  '11',
###                                  '12',
###                                  '21',
###                                  '22'
###                                ],
###                                [
###                                  '1',
###                                  '2',
###                                  '21',
###                                  '22'
###                                ],
###                                [
###                                  '1',
###                                  '11',
###                                  '12',
###                                  '2'
###                                ]
###                              ],
###               xdisjoint => [
###                              [
###                                0,
###                                0,
###                                0
###                              ],
###                              [
###                                0,
###                                0,
###                                0
###                              ],
###                              [
###                                0,
###                                0,
###                                0
###                              ]
###                            ],
###               xequivalent => [
###                                [
###                                  1,
###                                  0,
###                                  0
###                                ],
###                                [
###                                  0,
###                                  1,
###                                  0
###                                ],
###                                [
###                                  0,
###                                  0,
###                                  1
###                                ]
###                              ],
###               xsubset => [
###                            [
###                              1,
###                              0,
###                              0
###                            ],
###                            [
###                              0,
###                              1,
###                              0
###                            ],
###                            [
###                              0,
###                              0,
###                              1
###                            ]
###                          ],
###               xunique => [
###                            [
###                              '1',
###                              '2'
###                            ],
###                            [
###                              '11',
###                              '12'
###                            ],
###                            [
###                              '22',
###                              '21'
###                            ]
###                          ]
###             }, 'List::Compare::Multiple' )
