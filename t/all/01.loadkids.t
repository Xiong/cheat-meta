use Test::More;

use Bundle::Cheat;

{   
    my @kids    = Bundle::Cheat::_kids();
    plan tests  => scalar @kids;
    
    for my $kid (@kids) {
        local $@;
        eval ("require $kid");          # works
#        eval { require $kid };
#        eval { "require $kid" };
#        eval { require "$kid" };
#        print qq{$@\n};
#        print "now test...\n";
        ok( $@ =~ /OK/, qq{finding $kid} );
    };
    
};
