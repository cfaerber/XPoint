#! /bin/sh
echo getting cvs-source $1
pushd .
mkdir /tmp/xp-source-$1
cd /tmp/xp-source-$1
# remote
cvs -d :pserver:cvs@fries7-73.stw.uni-jena.de:/usr/local/cvs co -r branch_3_7_8 openxp

# local
#cvs co openxp

cd openxp
tar czf openxp-$1.tar.gz *
mv openxp-$1.tar.gz /usr/src/packages/SOURCES
echo now making rpm
#sleep 1
# echo press [enter] to continue
# read
popd
rpm -ba openxp-$1.spec
#rpmbuild -ba openxp.spec
rm -fr /tmp/xp-source-$1
