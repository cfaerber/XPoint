#!perl
# $Id$
#
# OpenXP >> Source >> Prepare sysdepend include files
# Copyright (C) 2003 by OpenXP/32 team <dev@openxp.de>
# Copyright (C) 2003 by Claus Faerber <claus@faerber.muc.de>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public
# License along with this program; if not, write to the Free
# Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
# USA.

use File::Find;

%systems = (
  'dos32'	  => 'DOS32',
  'win32'	  => 'Win32',
  'unix'	  => 'Unix',
  'unix/linux'	  => 'Linux',
  'unix/bsd'	  => 'BSD',
  'unix/bsd/free' => 'FreeBSD',
  'unix/bsd/net'  => 'NetBSD',
  'unix/bsd/open' => 'OpenBSD',
  'all'		  => undef,
);

%is_unixfs = (
  'dos32'	  => 0,
  'win32'	  => 0,
  'unix'	  => 1,
);

my %files = ();

find(sub{
    return if $File::Find::dir =~ m/^[\.\\\/]*$/;
    return if m/^\./ || m/~/ || !m/\.(pp|pas|inc)/;
    $File::Find::dir =~ s/[\\\/]+/\//g;
    $File::Find::dir =~ s/^\.\///;
    die "Unknown system: ".$File::Find::dir unless exists $systems{$File::Find::dir};

    $files{$_} = [] unless exists $files{$_};
    push @{$files{$_}}, $File::Find::dir.'/'.$_;
  }, '.');

foreach(keys %files) {
  my @f2 = @{$files{$_}};
  open FILE, ">$_";
  print FILE "(* \$Id$ *)\n";
  print FILE "(* Generated from source file tree - DO NOT EDIT *)\n";
  print FILE "\n";
  my $ifs = 0;
  
  foreach my $f (sort {
      $a =~ m/^(.*)\/([^\/]+)$/; my $ap = $1;
      $b =~ m/^(.*)\/([^\/]+)$/; my $bp = $1;
      return 1 if !defined $systems{$ap};
      return $bp cmp $ap;
    } @f2) {
      $f =~ m/^(.*)\/([^\/]+)$/; my $fp = $1;
      my $u = $fp; while(1) {
	if(exists $is_unixfs{$u}) { $u = $is_unixfs{$u}; last; }
	else { $u =~ s/\/?[^\/]*$//; if(!$u) { $u = undef; last; } }
      };
      printf FILE ('  'x$ifs++)."{\$IFDEF %s}\n", $systems{$fp}	if defined $systems{$fp};
      print  FILE ('  'x$ifs++)."{\$IFDEF UnixFS}\n"		unless defined $u;
      printf FILE ('  'x$ifs)  ."{\$I %s}\n", $f		unless defined $u && !$u;
      print  FILE ('  'x($ifs-1))."{\$ELSE}\n"    		unless defined $u;
      printf FILE ('  'x$ifs)  ."{\$I %s}\n", map({s/\//\\/g;$_;} $f) unless defined $u && $u;
      print  FILE ('  'x--$ifs)."{\$ENDIF}\n"    	 	unless defined $u;
      print  FILE ('  'x--$ifs)."{\$ELSE}\n"			if defined $systems{$fp};
      $ifs++							if defined $systems{$fp};
  }
  while($ifs-->0) {
    print FILE ('  ' x $ifs)."{\$ENDIF}\n";
  }
}
