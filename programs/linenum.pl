#!/usr/bin/perl
# $RCSfile: xref-words.perl,v $$Revision: 1.1 $

while( <> ){
   $hash{ $& } .= " $." while s/\w+//;
   };

   for $word( sort keys %hash ){
      print $word, $hash{ $word }, "\n";
      };
