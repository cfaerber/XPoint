#! /bin/sh
scriptdir=/home/boettger/openxp/script/
versiondir=/home/boettger/openxp/"$1"/
sourcedir="$versiondir"openxp/
subversion="$1".0

echo getting cvs-source $1
pushd .
mkdir /tmp/xp-source-$1
cd /tmp/xp-source-$1
# remote
echo version $1
case "$1" in
    3.8)
      echo "Checking out CVS for version $1 "
      cvs -d :pserver:cvs@fries7-73.stw.uni-jena.de:/usr/local/cvs co -r branch_3_7_8 openxp
      ;;
    3.9)
      echo "Checking out CVS for version $1 "
      cvs -d :pserver:cvs@fries7-73.stw.uni-jena.de:/usr/local/cvs co openxp
      ;;
    *)
      echo "No version given. Error"
      ;;
esac
# local
#cvs co openxp

cd openxp
tar czf "$versiondir"openxp-src-"$subversion".tar.gz *
mv "$versiondir"openxp-src-"$subversion".tar.gz /usr/src/packages/SOURCES/openxp-"$subversion".tar.gz
rm -fr /tmp/xp-source-$1
#echo now making rpm
sleep 5
# echo press [enter] to continue
# read
popd
rpm -ba "$scriptdir"openxp-"$subversion".spec
#rpmbuild -ba openxp.spec
