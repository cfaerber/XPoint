#!perl

# $Id$
# 
# Copyright (C) 1991-2001 Peter Mandrella
# Copyright (C) 2000-2001 OpenXP team (www.openxp.de)
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

open DE,"<openxp-d.rq";

while(<DE>) {
  next if /^[K#]/;
  next if /^$/;
  chomp;

  if(/^\+ *([0-9]*) *$/) {
    $cursect=$1;
  } elsif (/^- *$/) {
    $cursect=undef;
  } else {
    /^([0-9]*)[\t ]*([^\t ].*)/;
    $x=$1; $y=$2;
    
    $x=sprintf('%d.%d',$cursect,$x) if defined $cursect;
    $y=~s/([\x00-\x1F])/"'#".ord($1)."'"/gei;
    $y="'$y'"; $y=~s/^''//; $y=~s/''$//;
    
    $deutsch{$x}=$y;
  }
}

open EN,"<openxp-e.rq";

while(<EN>) {
  next if /^[K#]/;
  next if /^$/;

  if(/^\+ *([0-9]*) *$/) {
    $cursect=$1;
  } elsif (/^- *$/) {
    $cursect=undef;
  } else {
    /^([0-9]*)[\t ]*([^\t ].*)/;
    $x=$1; 
    $x=sprintf('%d.%d',$cursect,$x) if defined $cursect;
    delete $deutsch{$x};
  }
}

foreach (sort {$a <=> $b} keys %deutsch) {
  printf "%s (%s)\n",$_,$deutsch{$_};
}

#
# $Log$
# Revision 1.1  2001/09/12 21:39:39  cl
# - added programme to find missing ressources
#
