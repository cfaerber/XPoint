{
  XP version
}

unit xpversion;

interface

function xp_product: ansistring;        // product name: NewXP, OpenXP, CrossPoint, etc.
function xp_version: ansistring;        // product version
function xp_version_full: ansistring;   // extended product version

function xp_prver: ansistring;          // name and full version
function xp_prver_full: ansistring;     // name and full version

function xp_user_agent: ansistring;     // user agent
function xp_ftn_origin: ansistring;     // FTN origin
// function xp_ftn_product: integer;       // FTN product code (must be registered w/ FTSC)

function xp_copyright: ansistring;      // copyright holder

implementation

uses
  SysUtils,
  StrUtils,
  xposver;

const
  xp_xp		= 'NewXP';

  {$INCLUDE git-version.inc}

function xp_product: ansistring;
begin
  result := xp_xp;
end;

function xp_version: ansistring;
begin
  result := LeftStr(git_date, 10) + '.' + UpperCase(LeftStr(git_hash,8));
end;

function xp_version_full: ansistring;
begin
  result := xp_version;
end;

function xp_prver: ansistring;
begin
  result := xp_product + ' ' + xp_version;
end;

function xp_prver_full: ansistring;
begin
  result := xp_product + ' ' + xp_version_full;
end;

function xp_user_agent: ansistring;
begin
  result := xp_product + '/' + xp_version + ' (' +os_name + ' ' + os_release + '; ' + os_arch + ')';
end;

function xp_ftn_origin: ansistring;
begin
  result := xp_product + '/' + xp_version;
end;

// function xp_ftn_product: integer;       // FTN product code (must be registered w/ FTSC)

function xp_copyright: ansistring;     // maintainer name
begin
  result := '2001-' + LeftStr(git_date,4) + ' ' + git_name;
end;

end.

