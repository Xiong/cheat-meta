#!/run/bin/perl
#       bump.pl
#       = Copyright 2010 Xiong Changnian <xiong@cpan.org> =
#       = Free Software = Artistic License 2.0 = NO WARRANTY =

use 5.010001;
use strict;
use warnings;
use Carp;

#~ use Perl6::Junction qw( all any none one );
#~ use List::Util qw(first max maxstr min minstr reduce shuffle sum);
#~ use File::Spec;
use File::Spec::Functions qw(
    catdir
    catfile
    catpath
    splitpath
    curdir
    rootdir
    updir
    canonpath
);
use Cwd;
use Data::Lock qw( dlock dunlock );     # Lock variables
use Getopt::Long;                       # Process command line options
Getopt::Long::Configure ("bundling");   # enable, for instance, -xyz
use Pod::Usage;                         # Build help text from POD
use Text::Glob qw( glob_to_regex );     # Convert shell glob to Perl regex
use List::MoreUtils qw( any );          # The stuff missing in List::Util


#~ use Devel::Comments '###';
#~ use Devel::Comments '####';
#~ use Devel::Comments '#####';
use Devel::Comments '###', '#####';

#=========# Pseudo-globals

# Constants

# for Getopt::Long; also used to construct %dispatch table
dlock( my @getopt_setup     = qw(
    h+
    usage|use
    help
    man
    
    n|dryrun
    v|verbose
    b|bump=s
));
#    testrequired=i
#    testoptional:s
#    testnegatable!

dlock( my $do_prefix        = q{_do_}           );
dlock( my $ignore_file      = q{.gitignore}     );               


# Variables

my $getopt_okay     ;               # true if GetOptions() didn't fail
my %options         ;               # will be filled by GetOptions()
my @dispatch_setup  ;               # options processed in this order
my $dispatch        ;               # key is option; value is coderef
#~ my $write_files     ;               # if false, don't write any file
dlock(my $write_files = 0);         # locked off during development
my $verbose         ;               # if true, show lines (to be) changed
my $bump            ;               # new version; specify as string
my @ignore          ;               # list of regexes to ignore
my $ignore_regex    ;               # giant regex of alternates
my $recursion_depth = 0;            # safety counter

## Pseudo-globals

#----------------------------------------------------------------------------#
#---------# Main body

# Construct dispatch table
{
    no strict 'refs';
    #### @getopt_setup
    @dispatch_setup      = @getopt_setup;
    for (@dispatch_setup) {
        s/[[:^alpha:]].*$//;   # rm all from the first non-alpha to end
        $dispatch->{$_}     = \&{ $do_prefix . $_        };
    };
    $dispatch->{fail}       = \&{ $do_prefix . 'fail'    };
    $dispatch->{default}    = \&{ $do_prefix . 'default' };
    #### @dispatch_setup
    #### $dispatch
}

# Get command-line arguments
$getopt_okay = GetOptions(
    \%options,
    @getopt_setup,  
);

# Dispatch options
##### %options
if    ( not $getopt_okay ) {            # Getopt::Long failed
    $dispatch->{fail}->(@ARGV);
} 
elsif ( 
            join ( q{}, @ARGV )  eq q{}  
        and scalar keys %options == 0 
      ) {                               # empty command line
    $dispatch->{default}->();
} 
else  {
    for my $key (@dispatch_setup) {
        if ( exists $options{$key} ) {
            ### $key
            $dispatch->{$key}->( $options{$key} );
        };
    };
};

# Items to do loop
my @todo            ;
if ( join ( q{}, @ARGV )  eq q{} ) {    # nothing given to do
    _fail("No arguments given in command line to bump.");
}
else {
    @todo           = @ARGV;
};
for (@todo) {
    _alter_item($_);
};


exit(0);
#----------------------------------------------------------------------------#

#---------# Dispatch subroutines

sub _do_fail     { $dispatch->{usage}->(2); exit(1); };     # bad @ARGV

sub _do_default  { $dispatch->{usage}->(2); exit(1); };     # no  @ARGV

sub _do_h {                                 # process -h, -hh, -hhh
    my $level       = shift;
    
    given ($level) {
        when (1) { $dispatch->{usage}->(1); exit(0); }
        when (2) { $dispatch->{help}->();   exit(0); }
        when (3) { $dispatch->{man}->();    exit(0); }
        default  { $dispatch->{help}->();   exit(0); }        
    };
    
    return 0;
}; ## _do_h

sub _do_usage {                             # process --usage|-h
    my $level       = shift;
    my @text        = (
        q{Usage: bump [-n][-v] -b <version> [<file>|<folder>]+ },
        q{Type 'bump --help' for more info. },
    );
    given ($level) {
        when (1) { _talk(@text); exit(0); }
        when (2) { _yelp(@text); exit(1); }
        default  { _yelp(@text); exit(1); }
    };
    
    return 0;
}; ## _do_usage

sub _do_help {                              # process --help|-hh
    
    my %setup = (
#           -msg        => '#& bump: ',
            -exitval    => 'noexit',
            -verbose    => 1,
            -output     => \*STDOUT,
    );
    pod2usage(%setup);
    
    return 0;
}; ## _do_help

sub _do_man {                               # process --man|-hhh
    
    my %setup = (
#           -msg        => '#& bump: ',
#           -exitval    => 'noexit',
            -verbose    => 2,
            -output     => \*STDOUT,
    );
    pod2usage(%setup);
    
    return 0;
}; ## _do_man

sub _do_n {                                 # process -n|--dryrun
#~     $write_files = 0;
    return 1;
}; ## _do_n

sub _do_v {                                 # process -v|--verbose
    $verbose = 1;
    return 1;
}; ## _do_v

sub _do_b {                                 # process -b|--bump
    $bump = shift;
    #TODO Validate $bump: new version
    return 1;
}; ## _do_b


sub _alter_item {                           # alter file/folder; recursive
    my $item            = shift;
    my @subitems        ;
    if ( $recursion_depth++ > 1000 ) {
        _fail( "Recursion depth safety limit exceeded" );
    };
    
    _blacklist() if not @ignore;            # construct @ignore if needed
#~     _blacklist() if not $ignore_regex;      # construct if needed
    
    if ( any { $item =~ $_ } @ignore ) {
        _talk("$item -- Skipped.");
        return 0;        
    };
#~     if ($item =~ $ignore_regex) {
#~         _talk("$item -- Skipped.");
#~         return 0;
#~     };
    
    my $item_stat       = lstat $item;
    if    ( -d _ ) {
        opendir my $dh, $item
            or _fail(  "Failed to opendir $item" );
        @subitems       = readdir $dh
            or _fail(  "Failed to readdir $item" );
        closedir $dh
            or _fail( "Failed to closedir $item" );
        
        for (@subitems) {
            _alter_item( catfile( $item, $_ ) );                # recurse
        };
    } 
    elsif ( -f _ ) {
        _alter_file($item);                 # base case
    } 
    else {
        # something else; do nothing
    };
    
    return 1;
}; ## _alter_item

sub _alter_file {                           # alter a file

}; ## _alter_file

sub _blacklist {
#~     push @ignore, qr/^[.]{2}$/;            # parent ../
    open my $fh, '<', $ignore_file
            or _fail(   "Failed to open $ignore_file for reading ");
    while (<$fh>) {
        given ($_) {
            when (/^#/)         { }         # skip comment
            when (/^[\s]*$/)    { }         # skip blank
            default             {
                chomp;
                push @ignore, glob_to_regex($_);
            }
        };
    };
    close $fh;
    ##### @ignore
    $ignore_regex       = qr//;
    for (@ignore) {
        $ignore_regex   = qr/$ignore_regex|$_/;
    };
    ### $ignore_regex
    return 1;
}; ## _blacklist


sub _talk { _say_to_screen( \*STDOUT, @_ ) };                   # ~say

sub _yelp { _say_to_screen( \*STDERR, @_ ) };                   # ~warn

sub _fail { _say_to_screen( \*STDERR, @_ ); croak() };     # ~die

sub _say_to_screen {
    my $outfh       = shift;                        # required
    my $intro       = q{#& };                       # introduce each line
    my $prepend     = join q{}, $intro, $0, q{: };  # prepend to all talk               
       
    my $indent      = qq{\n} . $intro 
                    . q{ } x ( (length $prepend) - (length $intro) )
                    ;
    
    my $first_line  = $prepend . shift;
    unshift @_, $first_line;
    my $text        = join $indent, @_; # and indent remaining lines
    
    # Say it
    say {$outfh} $text;
    
    return 1;
}; ## _say_to_screen

__END__

=head1 NAME

bump - Set version numbers in several files

=head1 SYNOPSIS

    $ bump [-n][-v] -b <version> [<file>|<folder>]+ 

=head1 OPTIONS

=over

=item -b|--bump <version>

    $ bump -b '1.2.3'

Required argument. Version-specifying lines will be bumped to this.

=item [-n|--dryrun]

Show what would be done. Implies --verbose.

=item [-v|--verbose]

Print each line changed. 

=item [-h|--usage]

Prints a short usage summary.

=item [-hh|--help]

Prints all available options.

=item [-hhh|--man]

Displays all documentation.

=back

=head1 ARGUMENTS

    $ bump -b '1.2.3' lib foo.pm

Remaining command-line arguments specify files and folders to be bumped. 
Folders will be searched recursively. Symlinks will not be followed. 

=head1 AUTHOR

Xiong Changnian  C<< <xiong@cpan.org> >>

=head1 LICENSE

Copyright (C) 2010 Xiong Changnian C<< <xiong@cpan.org> >>

This library and its contents are released under Artistic License 2.0:

L<http://www.opensource.org/licenses/artistic-license-2.0.php>

=cut

