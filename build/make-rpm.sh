#! /bin/sh
# has to be called in main source dir for desired version
cd build
eval $(./get_build_nr.pl)
mainversion=$OPENXP_MAINVER
subversion=$OPENXP_SUBVER
buildnr=$OPENXP_BUILD
version="$mainversion"."$subversion"-"$buildnr"
scriptdir=/home/boettger/openxp/script/
versiondir=/home/boettger/openxp/"$mainversion"/
sourcedir=$versiondir'openxp/'
cd "$sourcedir"
#
# example: version = 3.8.12-1
# mainversion has to be 3.8 in this example
# subversion has to be 12 in this example
# buildnr has to be 1 in this example

echo getting cvs-source $mainversion
pushd .
mkdir /tmp/xp-source-$mainversion
cd /tmp/xp-source-$mainversion
# remote
echo version $version
case "$mainversion" in
    3.8)
      echo "Checking out CVS for version $version "
      cvs -d :pserver:cvs@openxpcvs.dyndns.org:/usr/local/cvs co -r branch_3_7_8 openxp
      ;;
    3.9)
      echo "Checking out CVS for version $version "
      cvs -d :pserver:cvs@openxpcvs.dyndns.org:/usr/local/cvs co openxp
      ;;
    *)
      echo "No version given. Error"
      ;;
esac
# local
#cvs co openxp

cd openxp
tar czf "$versiondir"openxp-src-"$version".tar.gz *
mv "$versiondir"openxp-src-"$version".tar.gz /usr/src/packages/SOURCES/openxp-"$version".tar.gz
rm -fr /tmp/xp-source-$mainversion
#echo now making rpm
sleep 5
# echo press [enter] to continue
# read
popd
rpm -ba "$sourcedir"/build/openxp-"$version".spec
#rpmbuild -ba openxp.spec
