package Bundle::Cheat::Util;

BEGIN {
    die 'OK';
}

## END MODULE
return 1;   # Abort execution; fool syntax highlighting for following code.
die 'Failed to abort. This line should never execute; please contact author.';

#============================================================================#
#=========# UTILITY MODULES
                                # [<-44 cols to end                78 cols ->]
                                # #2345678901234567890123456789012345678901234

use Scalar::Util;               # General-utility scalar subroutines
use Scalar::Util qw(
    weaken isweak reftype refaddr blessed isvstring readonly tainted 
    dualvar looks_like_number openhandle set_prototype 
);
    weaken $ref;            # $ref will not keep @$ref, %$ref, etc. from GC
                            # note: copies of weak refs are not weak
    $bool = isweak  $ref;       # true if $ref is a weak reference
    $type = reftype $ref;       # 'SCALAR', 'ARRAY', 'HASH', or undef
    $addr = refaddr $ref;       # machine address of $ref or undef
    $got  = blessed $ref;       # class of blessed ref or undef 
    $bool = isvstring $s;       # true if $s is a v-string
    $bool = readonly  $s;       # true if $s is a readonly scalar
    $bool = tainted   $s;       # true if $s is tainted
    $got  = dualvar $num, $string;      # $got is $num or $string in context 
    $bool = looks_like_number $n;       # true if $n can be a number
    $fh   = openhandle $t_fh;       # $h if $t_fh is a tied or open filehandle
    set_prototype $cref, $proto;        # sets prototype of &$cref to $proto
## Scalar::Util

use List::Util;                 # General-utility list subroutines
use List::Util qw( max maxstr min minstr first reduce shuffle sum );
    $got  = max    @a;          # returns item >  than all the rest
    $got  = maxstr @a;          # returns item gt than all the rest
    $got  = min    @a;          # returns item <  than all the rest
    $got  = minstr @a;          # returns item lt than all the rest
    $got  = first  {$_} @a;     # ~grep but returns only first true item 
    $got  = reduce { $bool?$a:$b } @a;  # returns one item; last man standing 
    $got  = sum @a;             # sum of all elements
    @gots = shuffle @a;         # pseudo-randomizes order of @a
    # "The following are additions that have been requested..."
    sub any { $_ && return 1 for @_; 0 };       # One argument is true
    sub all { $_ || return 0 for @_; 1 };       # All arguments are true
    sub none { $_ && return 0 for @_; 1 };      # All arguments are false
    sub notall { $_ || return 1 for @_; 0 };    # One argument is false
    sub true { scalar grep { $_ } @_ };         # How many elements are true
    sub false { scalar grep { !$_ } @_ };       # How many elements are false
## List::Util

use List::MoreUtils ':all';     # The stuff missing in List::Util
use List::MoreUtils qw(
    any all none notall true false firstidx first_index 
    lastidx last_index insert_after insert_after_string 
    apply after after_incl before before_incl indexes 
    firstval first_value lastval last_value each_array
    each_arrayref pairwise natatime mesh zip uniq minmax
);
    # These operators take a block (~grep), setting $_ to each item in @a
    # Your block should test $_ and return a $bool
    $bool  = any    {$_} @a;    # true if any test  is  true   (  $A ||  $B )
    $bool  = all    {$_} @a;    # true if all tests are true   (  $A &&  $B )
    $bool  = none   {$_} @a;    # true if all tests are false  ( !$A && !$B )
    $bool  = notall {$_} @a;    # true if any test  is  false  ( !$A || !$B )
    #   #   #   #   #   #   De Morgan's Laws:    #   #   #   #   #   #   #   # 
    # ( !$A && !$B ) == !(  $A || $B  ) and ( !$A || !$B ) == !(  $A &&  $B )
    # (  $A &&  $B ) == !( !$A || !$B ) and (  $A ||  $B ) == !( !$A && !$B )
    $count = true   {$_} @a;        # how many true tests
    $count = false  {$_} @a;        # how many false tests
    $got  = firstidx{$_} @a;        # first item with true test
    $got  = first_index             # ditto; alias firstidx
    $got  = lastidx {$_} @a;        # last item with true test
    $got  = last_index              # ditto; alias lastidx
    $got  = insert_after {$_} $v => @a;   # put $v in @a after first true test        
    $got  = insert_after_string $s, $v => @a;  # insert after first item eq $s
    @gots = apply       {$_} @a;    # ~ map but doesn't modify @a
    @gots = after       {$_} @a;    # ( c )    ~~ after       {/b/} (a, b, c)
    @gots = after_incl  {$_} @a;    # ( b, c ) ~~ after_incl  {/b/} (a, b, c)
    @gots = before      {$_} @a;    # ( a )    ~~ before      {/b/} (a, b, c)
    @gots = before_incl {$_} @a;    # ( a, b ) ~~ before_incl {/b/} (a, b, c)
    @gots = indexes     {$_} @a;    # ( 1 )    ~~ indexes     {/b/} (a, b, c)
    $got  = firstval    {$_} @a;    # ~List::Util::first() but -1 on fail
    $got  = first_value             # ditto; alias firstval
    $got  = lastval     {$_} @a;    # last item testing true
    $got  = last_value              # ditto; alias lastval
    $cref = each_array @a, @b, @c;  # creates an n-tuplewise iterator closure
    while ( my ($A, $B, $C) = $cref->() ) {     # returns empty list 
        # Your code here                        #   when all lists exhausted
    };
    $cref = each_arrayref @a, @b, @c;   # iterator returns refs to arg lists
    $cref = natatime $n, @a;    # creates $n-at-a-time iterator from one list
    @gots = pairwise    {$_} @a, @b;    # ~map over two arrays
    @gots = mesh @a, @b, @c;    # ( $a[0], $b[0], $c[0], $a[1], $b[1], $c[1] )
    @gots = zip  @a, @b, @c;    # ditto; alias mesh
    @gots = uniq @a;            # returns *only* unique elements
    ( $min, $max )  = minmax @a;    # ~( List::Util::min(@a), ::max(@a) )
    @refs = part { $p = f($_) } @a; # partitions @a into multiple lists
    # you return integer $p as index of @refs; @refs is a list of arrayrefs
## List::MoreUtils

use List::AllUtils qw( :all );  # Everything from List::Util, List::MoreUtils
    
use List::Compare;              # Compare elements of two or more lists
# This object-oriented module is highly orthogonal, 
#   so that nearly any selection or choice may be combined with any other. 
# Most methods are equally okay for ( just two ) or ( three or more ) lists; 
#   some will Carp if called inappropriately.
#                            === Options/Modes === 
#           Regular: (default) Two lists, sorted results, many method calls ok
#          Unsorted: Don't sort method results              ( faster ) 
#       Accelerated: Only one method call possible per $lc  ( faster )
#          Multiple: Three or more lists in constructor 
# ! (specify $ix to refer to a given list; default 0; omit for only two lists)
#         Seen-hash: Use hashrefs instead of arrayrefs: 
#   [ 11, 12, 14, 14, 14, 15 ] ~~ { 11 => 1, 12 => 1, 14 => 3, 15 => 1 }
#
    # Construct a work-object 
    my $lc = List::Compare->new(             \@a, \@b, @c );  # default
    my $lc = List::Compare->new( '-u',       \@a, \@b, @c );  # unsorted
    my $lc = List::Compare->new(       '-a', \@a, \@b, @c );  # accelerated
    my $lc = List::Compare->new( '-u', '-a', \@a, \@b, @c );  #! -u before -a
    # Wrap constructor arguments in a hashref
    my $lc = List::Compare->new({
        unsorted    => 1,
        accelerated => 1,
        lists       => [ \@a, \@b, @c ],
    });
    # Methods return lists of results
    @gots = $lc->get_intersection;          # found in each/both list(s)
    @gots = $lc->get_union;                 # found in any/either list
    @gots = $lc->get_bag;                   # ~get_union but also duplicates
    @gots = $lc->get_unique($ix);           # found only in list $ix
    @gots = $lc->get_complement($ix);       # not found in $ix, but elsewhere
    @gots = $lc->get_symmetric_difference;  # found in only one list
    $gots = $lc->get_intersection_ref;          # ~methods above but
    $gots = $lc->get_union_ref;                 #           returns \@gots
    $gots = $lc->get_bag_ref;                   #       "
    $gots = $lc->get_unique_ref($ix);           #       "
    $gots = $lc->get_complement_ref($ix);       #       "
    $gots = $lc->get_symmetric_difference_ref;  #       "
    # Methods return boolean truth      # ( $ixL, $ixR ) default to ( 0, 1 )
    $bool = $lc->isLsubsetR( $ixL, $ixR );      # true if all L in R
    $bool = $lc->isRsubsetL( $ixL, $ixR );      # true if all R in L
    $bool = $lc->isLequivalentR( $ixL, $ixR );  # true if same items in L, R
    $bool = $lc->isLdisjointR( $ixL, $ixR );    # true if no items in common
    # Methods return list of which lists ($ix) satisfying conditions
    @ixs  = $lc->is_member_which($string);      # some item in $ix eq $string
    @ixs  = $lc->are_members_which(\@strings);  # ~prev but eq any in @strings
    # Dump
    $lc->print_subset_chart;        # pretty-print tables showing some
    $lc->print_equivalence_chart;   #   relationships; row/col as $ix
# List::Compare

use Hash::Util;                 # Hash key, value locking
use Hash::Util qw(
    lock_keys lock_keys_plus unlock_keys 
    lock_value unlock_value 
    lock_hash unlock_hash lock_hash_recurse unlock_hash_recurse
    hash_locked hidden_keys legal_keys all_keys 
);
    # Restrict %hash to a set of keys; can delete but can't add other keys
    \%hash = lock_keys     ( %hash );           # current keys %hash
    \%hash = lock_keys     ( %hash, @keys );    # @keys; subset of keys @hash
    \%hash = lock_keys_plus( %hash, @keys );    #        superset
    \%hash = unlock_keys   ( %hash );           # remove restrictions
    # Cannot alter value of $key but can delete the k/v pair
    \%hash = lock_value    ( %hash, $key );
    \%hash = unlock_value  ( %hash, $key );
    # Lock the whole %hash; can't add, delete, or change value at all
    \%hash = lock_hash              ( %hash );
    \%hash = unlock_hash            ( %hash );
    \%hash = lock_hash_recurse      ( %hash );  # HoHoH... only
    \%hash = unlock_hash_recurse    ( %hash );  #   ditto
    # Other functions...
    $bool  = hash_unlocked ( %hash );       # true if %hash is unlocked
    @keys  = legal_keys    ( %hash );       # list of keys allowed
    @keys  = hidden_keys   ( %hash );       # see docs; experimental feature
    \%hash = all_keys( %hash, @keys, @hidden );       # experimental feature
    # Just like Daddy but take hashref arguments
    \%hash = lock_ref_keys          ( \%hash );
    \%hash = lock_ref_keys          ( \%hash, @keys );
    \%hash = lock_ref_keys_plus     ( \%hash, @keys );
    \%hash = unlock_ref_keys        ( \%hash );
    \%hash = lock_ref_value         ( \%hash, $key );
    \%hash = unlock_ref_value       ( \%hash, $key );
    \%hash = lock_hashref           ( \%hash );
    \%hash = unlock_hashref         ( \%hash );
    \%hash = lock_hashref_recurse   ( \%hash );
    \%hash = unlock_hashref_recurse ( \%hash );
    $bool  = hash_ref_unlocked      ( \%hash );
    @keys  = legal_ref_keys         ( \%hash );
    @keys  = hidden_ref_keys        ( \%hash );
## Hash::Util









use Test::Simple tests => 6;    # Basic utilities for writing tests
    ok( $bool, $name );             # ok if $bool is true
    ok( $foo eq $bar, $name );      # ok if $foo eq $bar
## Test::Simple

use Test::More tests => 6;      # Standard framework for writing test scripts
    ok  ( $bool, $name );               # ok if $bool is true
    is  ( $got, $want, $name );         # ok if $got eq $want
    isnt( $got, $want, $name );         # ok if $got ne $want
    like( $got, qr/./, $name );         # ok if $got =~ /regex/
  unlike( $got, qr/./, $name );         # ok if $got !~ /regex/
  cmp_ok( $got, '==', $want, $name );    # ok if $got == $want
  my $object = new_ok( $class => \@args );  # calls $class->new(@args)
  can_ok( $object, @methods );              # ...or: can_ok($module...
  isa_ok( $object, $class, $object_name);   # safe if $object is undef
    subtest $name => \&code;            # version 0.94
    pass( $name);                   # unconditional ok
    fail( $name);                   # unconditional failure
    BEGIN { use_ok($module, @imports); }    # ok if find, load, import
    require_ok($file);                      # ok if find and load
    is_deeply( $got, $want, $name );    # walks deeply but doesn't check bless
use Test::More;                     # declare number of tests later
    plan tests => $calculated;      # calculate plan at run time
    done_testing($counter);         # or after testing
    diag(@message);             # will print but won't mess up test harness
    note(@message);             # will only print if verbose output is asked
    ok($bool) or diag(@message);    # passage or failure propagates
    my @dump = explain( @refs );            # uses Data::Dumper
    my @dump = explain( \@array, \%hash );  #   to dump list of references
    BAIL_OUT( $reason );        # abort this and all following test scripts
## Test::More

use Test::Deep;                 # Extremely flexible deep comparison
    cmp_deeply( $got, $want, $name );   # ok if $got eq $want deeply
    # Special comparision functions for each value; may be nested
    my $cmp = {                     # check each $got->{key}        # $gv...
        key     => ignore(),            # ok regardless of $gv
        key     => 'literal',           # ok if $gv eq 'literal'
        key     => re('regex'),         # ok if $gv =~ /regex/
        key     => bag(@want),          # ignore ordering of elements
        key     => set(@want),          # ignore ord.of and duplicate elements
        key     => superbagof(@want),   # $gv contains at least this bag
        key     => subbagof  (@want),   # $gv contains at most  this bag
        key     => supersetof(@want),   # $gv contains at least this in order
        key     => subsetof  (@want),   # $gv contains at most  this in order
        key     => all(@want),          # ok if $gv eq all @want (and)
        key     => any(@want),          # ok if $gv eq any @want (or)
        key     => array_each($cmp2),   # check each @{$gv} against $cmp2
        key     => str ($want),         # stringify $gv eq $want
        key     => num ($want, $tolc),  # numify    $gv == $want +/- $tolc
        key     => bool($want),         # ok if ( !!$gv == !!$want )
        key     => code($cref),         # $c = sub( $gv = shift; return $ok );
        key     => isa($class),         # ok if $gv->UNIVERSAL::isa($class)
        key     => methods(             # invoke methods of $gv
            method => $want,            # ok if $gv->method()      eq $want
          [ method, @args ] => $want,   # ok if $gv->method(@args) eq $want
        ),
    };
    cmp_deeply( $got, $cmp,  $name ); # ok if $got special $cmp deeply
## Test::Deep

use Test::Trap;                 # Trap exit codes, exceptions, output, etc.
# $trap object is exported into your namespace and contains everything.
# Methods can be combined in a large variety of ways; see Test::Trap POD.
use Test::Trap  qw( :raw :die :exit 
                        :flow 
                    :stdout             :stderr 
                    :stdout(perlio)     :stderr(perlio)
                    :stdout(tempfile)   :stderr(tempfile)
                    :stdout(method)     :stderr(method)
                    :stdout( m1, m2, m3 )
                    :warn
                        :default
                    :on_fail(method)
                    :void       :scalar     :list
                    :output(systemsafe)     :output(method)
);
use Test::Trap(     # order of layers in the use-array is significant
        ':raw',                 # traps normal return and stops trapping
        ':die',                 # traps fatal exceptions
        ':exit',                # traps attempts to exit() perl
    ':flow',                # shortcut for :raw:die:exit
        ':stdout',              # trap STDOUT
        ':stderr',              # trap STDERR
            ':stdout(perlio)',      # trap using PerlIO::scalar
            ':stdout(tempfile)',    # trap using File::Temp to a tempfile
            ':stdout(method)',      # trap using some user method
            ':stdout(m1,m2,m3)',    # provide a list of fallback methods
        ':warn',                # trap warnings and tee them to STDERR
    ':default',             # shortcut for :raw:die:exit:stdout:stderr:warn
        ':on_fail(method)',     # user method to callback on fatals
    ':void',                # return in scalar context
    ':scalar',              # return in list   context
    ':list',                # return in void   context
        ':output(systemsafe)',  # traps children including system calls
        ':output(method)',      # fallback processing with user method
);
    # If you want the normal return value from code under test, 
    #       use Test::Trap qw( :scalar );
    #       use Test::Trap qw( :list );
    #   or  provide a context yourself with:
    my $rv  = trap{  };     # return in scalar context
    my @rvs = trap{  };     # return in list   context
    trap{                   # return in void   context
        # Your code under test here
    };
    $trap->diag_all;                    # Dumps the $trap object, TAP safe
    # Accessor methods  # ACC
    my $got = $trap->leaveby;           # 'return', 'die', or 'exit'. 
    my $got = $trap->die;               # exception thrown if any 
    my $got = $trap->exit;              # exit code caught if attempted
    my $got = $trap->stdout;            # STDOUT in one string
    my $got = $trap->stderr;            # STDERR in one string
    my $got = $trap->return;            # arrayref of normal return values
    my $got = $trap->return($index);    # pass $index as method argument
    my $got = $trap->return(@indices);  # it slices, it dices
    my $got = $trap->warn  ($index);    # warnings as an array
    # Test methods  (for any ACC)
    #   e.g.:       $trap->return_ok();   $trap->stdout_like();
    #   $ix or @ixs required if ACC is array, otherwise omit
    use Test::More tests => 9;                      # ~~ Test::More::*
    $trap->ACC_ok       ( $ix, $name );             #              ok()
    $trap->ACC_nok      ( $ix, $name );             #       !      ok()
    $trap->ACC_is       ( $ix, $want,  $name );     #              is()
    $trap->ACC_isnt     ( $ix, $want,  $name );     #       !      is()
    $trap->ACC_like     ( $ix, qr/./,  $name );     #            like()
    $trap->ACC_unlike   ( $ix, qr/./,  $name );     #       !    like()
    $trap->ACC_isa_ok   ( $ix, $class, $name );     #             isa()
    $trap->ACC_is_deeply( $ix, $want,  $name );     #       is_deeply()
    # Examples of above: 
    $trap->return_ok( 0, 'got something' ); # even if :scalar, return => []
    $trap->return_is( 1, 9,  'returns an array and the second element is 9' );
    $trap->stdout_like( qr/hell|damn/, 'tried to print a word to screen'); 
    $trap->die_unlike ( qr/hell|damn/, 'died like a lady');  # fail if !die
    $trap->return_isa_ok ( 0, 'Acme::Teddy' 'returned object');
    # Convenience methods with better diagnostics than their equivalents...
    $trap->did_die;         # $trap->leaveby_is('die');
    $trap->did_exit;        # $trap->leaveby_is('exit');
    $trap->did_return;      # $trap->leaveby_is('return');
    $trap->quiet;           # ok( !$trap->stdout && !$trap-stderr);
## Test::Trap

#TODO: use Test::Exception tests => 6; # Test exception based code




#============================================================================#
__END__

=head1 NAME

Bundle::Cheat::Util - Usage for utility modules

=head1 SYNOPSIS

    $ cat Bundle/Cheat/Test.pm

=head1 DESCRIPTION

I<The test itself is a cheat, isn't it?> 
I<I mean you program it to be unwinnable.> 
--James Tiberius Kirk

This is a collection of "cheat sheets": highly compressed, abbreviated 
documentation for various modules. Each module within the bundle covers a 
top-level namespace or a set of otherwise closely-related modules. 

For each module, a paragraph is given, generally: 

    Some::Module            # Short description
        qw( various exportable symbols if any );
        routine( $placeholder, @arguments );
        $context    = function( @arguments);
        $object->method();

You should be able to copy and paste this into your own code, 
delete what you don't need, and be on your way. 

=head1 CHEATS

=over

=item * L<Test::Simple>

=item * L<Test::More>

=item * L<Test::Deep>

=item * L<Test::Trap>

=back

=head1 SEE ALSO

=over

=item * L<perlcheat>

=item * L<Bundle::Cheat>

=back

=head1 FUNCTIONS/METHODS

None. This module does nothing at all. 
You shouldn't even try to C<use()> it;
if you try, it will C<die()>.

If it didn't C<die()> immediately, in an early C<BEGIN> block, 
then it would try to C<use()> every single module it cheats. 
So we don't do that. 

=head1 INTERFACE 

Open the module (the *.pm file itself) in your editor. 
Copy out whatever you like. 

=head1 DIAGNOSTICS

=over

=item Failed to abort. This line should never execute; please contact author.

This module contains executable code so that your editor will use
its syntax highlighting feature, if any. But it would be madness to execute it
all. You should never, ever see this error. 

=back

=head1 CONFIGURATION AND ENVIRONMENT

Bundle::Cheat::Util requires no configuration files or environment variables.

=head1 DEPENDENCIES

No dependencies. 

=head1 INCOMPATIBILITIES

None.

=head1 BUGS AND LIMITATIONS

No cheat sheet will teach you anything. It's only a reminder. You B<must> 
consult each module's own full documentation I<at least> before using it. 
I hope. 

This module does not contain magic to squirt code into your module. 
Copy and paste. 

=head1 THANKS

=over

=item *

To about 8500 authors who have uploaded about 85,000 modules to the CPAN. 

=back

=head1 AUTHOR

Xiong Changnian  C<< <xiong@cpan.org> >>

=head1 LICENSE

Copyright (C) 2010 Xiong Changnian C<< <xiong@cpan.org> >>

This library and its contents are released under Artistic License 2.0:

L<http://www.opensource.org/licenses/artistic-license-2.0.php>

=cut
