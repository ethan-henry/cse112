perl -MO=Xref, -oreport myperlprogram

#MOST LIKELY A BASH LIKE COMMAND


#xref-idents


#!/usr/bin/perl
# $RCSfile: solution_asgt1.perl,v $$Revision: 940420.1 $
#

$alphaset = "A-Za-z_";

$file = $ARGV[0];
while( <> ){
   $words{ "$& [$file]" } .= " $." while s/[$alphaset][0-9$alphaset]*//;
      close( ARGV ), $file = $ARGV[0] if eof;
      };

      for $word( sort keys %words ){
         print $word, $words{ $word }, "\n";
         };



#xref-words

#!/usr/bin/perl
# $RCSfile: solution_asgt1.perl,v $$Revision: 940420.1 $
#

sub lex{
   ( $aa = $a ) =~ s/.*/\L$&/;
   ( $bb = $b ) =~ s/.*/\L$&/;
   return ( $cc = $aa cmp $bb ) ? $cc : $a cmp $b;
};

while( <> ){
    $words{ $& } .= " $." while s/\w+//;
};

for $word( sort lex keys %words ){
    $line = sprintf( "%-19s", $word ) . $words{ $word };
    while( $line =~ /.{80}/ ){
        $line =~ s/^(.{1,79}) /' ' x 20/e
        || $line =~ s/^([^ ]+) /' ' x 20/e;
        print $1, "\n";
    };
    print $line, "\n";
};  



#xref-words.perl

#!/usr/bin/perl
# $RCSfile: xref-words.perl,v $$Revision: 1.1 $

while( <> ){
    $hash{ $& } .= " $." while s/\w+//;
};

for $word( sort keys %hash ){
    print $word, $hash{ $word }, "\n";
};                                              
