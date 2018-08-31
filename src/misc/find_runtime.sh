#! /bin/sh
# Guess the location of the sml runtime system 
prog=`which sml-cm`
case `basename $prog` in
 sml-cm.bat)
   #icck don't ask...
    runtime=`echo $prog | sed -e 's:sml-cm.bat:.run/run.x86-win32.exe:g'`
 ;;
 *) runtime=`sh -x $prog 2>&1 < /dev/null | awk '/.*exec/ { print $3;}' ` 
 ;;
esac
name=`basename ${runtime}`
echo $runtime 
