#!/bin/sh

set -e

x() { if test -n "${TRACE}"; then echo "$@"; fi
      "$@"
}
q() { if test -n "${TRACE}"; then echo "$@"; fi
      "$@" >/dev/null
}
fail() { echo "FAIL: $1"; exit 1
}
pass() { echo "OK: $1"
}
build_should_fail() {
        echo
        nh build --reuse-cache "$1" >/dev/null 2>&1 || true
        local EXPFAIL="$2"
        local EXPFAILTY="$3"
        local ACTFAIL=$(  nh failure      "$1" || true)
        local ACTFAILTY=$(nh failure-type "$1" || true)

        if   test -z "${ACTFAIL}"
        then fail "not failed>  $1"
        elif test "${EXPFAIL}" != "${ACTFAIL}"
        then fail "wrong failure of $1>  expected $EXPFAIL, yet failed as $ACTFAIL/$ACTFAILTY"
        elif test -n "${EXPFAILTY}" -a "${EXPFAILTY}" != "${ACTFAILTY}"
        then fail "misfail $1>  $ACTFAIL/$ACTFAILTY -- expected $EXPFAIL/$EXPFAILTY"
        else pass "proper fail $1>  $EXPFAIL${EXPFAILTY:+/$EXPFAILTY}"
        fi
}
build_should_pass() {
        echo
        if ! nh build "$1"
        then fail "did not pass>  $1', $(nh failure "$1")/$(nh failure-type "$1")"
        else pass "proper pass>  $1"
        fi
}
property_is() {
        echo
        attr="$1"; type="$2"; prop="$3"; expected="$4"
        actual="$(nh-def x get "$type" "$prop" "$attr" UNDEFINED)"
        if test "$expected" !="$actual"
        then fail "property $attr.$type.$prop expected>  '$expected', actual '$actual'"
        else pass "property $attr.$type.$prop as expected>  '$expected'"
        fi
}
eval_is() {
        echo
        expr="$1"; expected="$2"
        actual="$(nh x $expr)"
        if test "$expected" != "$actual"
        then fail "'$expr' evaluated to: '$actual', expected: '$expected'"
        else pass "'$expr' as expected is: '$expected'"
        fi
}

debug=
pkgdb=
silent=
while test $# -ge 1
do case "$1"
   in --cls )                  echo -en "\ec";;
      --pkgdb )                pkgdb="$2"; shift;;
      --silent )               silent="--silent";;
      --trace )                TRACE="--trace";;
      --debug )                set -x; export NH_DEBUG="--debug";;
      "--"* )                  fail "$0: unknown option: $1";;
      * )                      break;;
   esac
   shift
done
MAYCMD="$1"
if test -n "${MAYCMD}"; then shift; fi
case "$MAYCMD" in
        x ) "$@"; exit $?;;
esac

###
### main :: IO ()
###
cd tests 2>/dev/null || true

db="${pkgdb:-$(mktemp -d /tmp/nh-test-area-XXXXXXXXX)}"
atexit() {
        if test -z "${pkgdb}"
        then rm -rf ${db}
        fi
}
trap atexit EXIT

cat > .nh <<EOF
GHC=841
PKGDB=${db}
NIX_GHC_OVERRIDES=overrides.nix        # The job of 'nh' is to help you maintain this.
NIX_GHC_PACKAGE_SET=packages.nix       # Define a proper GHC package set.
EOF

###
### Set up / smoke test
###
x nh init "${db}" --force
x nh ls
x nh overrides

######################################################################################################
###                                                                                                ###
###  Test suite starts here                                                                        ###
###                                                                                                ###
######################################################################################################
###
### Define packages by their interesting properties.
### Also, define their fixes, but keep them inactive
###
WORKING=data-default

BROKEN_SHADOWED=vector-space
x nh disable        ${BROKEN_SHADOWED}
x nh set-upstream   ${BROKEN_SHADOWED} conal
q nh hackage        ${BROKEN_SHADOWED}

BROKEN_UNMERGED=composition-tree
x nh dontCheck      ${BROKEN_UNMERGED}
q nh unmerged       ${BROKEN_UNMERGED} deepfire ""
x nh disable        ${BROKEN_UNMERGED} src

###
### Actual testing
###

## Simple positive baseline
build_should_pass ${WORKING}

## Shadowing makes positive change
x eval_is           "status ${BROKEN_SHADOWED}" shadowed
x build_should_fail ${BROKEN_SHADOWED} DIRECT
x nh enable         ${BROKEN_SHADOWED}
x build_should_pass ${BROKEN_SHADOWED}

## Unmerged
x eval_is           "status ${BROKEN_UNMERGED}" unmerged
x build_should_fail ${BROKEN_UNMERGED} DIRECT CABAL-MISSING-DEPS
x nh enable         ${BROKEN_UNMERGED} src
x build_should_pass ${BROKEN_UNMERGED}
