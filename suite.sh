#!/bin/sh

set -e

SILENT=yes

build() {
          nh info "$1"
          if test -n "$SILENT"
          then nix-build packages.nix --no-out-link -A "$1" >/dev/null 2>/dev/null
          else nix-build packages.nix --no-out-link -A "$1"
          fi
}
fail() { echo "FAIL: $1"; exit 1
}
pass() { echo "OK: $1"
}
build_should_fail() {
        if   build "$1"
        then fail "build should fail: '$1'"
        else pass "build failed, as expected: '$1'"
        fi
}
build_should_pass() {
        if ! build "$1"
        then fail "build should pass: '$1'"
        else pass "build passed, as expected: '$1'"
        fi
}
property_is() {
        attr="$1"; type="$2"; prop="$3"; expected="$4"
        actual="$(nh-def x get "$type" "$prop" "$attr" UNDEFINED)"
        if test "$expected" !="$actual"
        then fail "property $attr.$type.$prop expected: '$expected', actual '$actual'"
        else pass "property $attr.$type.$prop as expected: '$expected'"
        fi
}
eval_is() {
        expr="$1"; expected="$2"
        actual="$(nh x $expr)"
        if test "$expected" != "$actual"
        then fail "'$expr' evaluated to: '$actual', expected: '$expected'"
        else pass "'$expr' as expected is: '$expected'"
        fi
}

db="$(mktemp -d /tmp/nh-test-area-XXXXXXXXX)"
atexit() {
        rm -rf ${db}
}
trap atexit EXIT

cd tests 2>/dev/null || true

cat > .nh <<EOF
GHC=841
PKGDB=${db}
NIX_GHC_OVERRIDES=overrides.nix        # The job of 'nh' is to help you maintain this.
NIX_GHC_PACKAGE_SET=packages.nix       # Define a proper GHC package set.
EOF

WORKING=data-default
BROKEN_SHADOWED=vector-space
BROKEN_UNMERGED=composition-tree
###
### Set up
###
nh init "${db}" --force
nh ls
nh overrides

### Define the fixes, but keep them inactive
nh disable        ${BROKEN_SHADOWED}
nh set-upstream   ${BROKEN_SHADOWED} conal
nh hackage-up     ${BROKEN_SHADOWED}

nh disable        ${BROKEN_UNMERGED}
nh dontCheck      ${BROKEN_UNMERGED}
nh jailbreak      ${BROKEN_UNMERGED}  ## trimmable
nh unmerged       ${BROKEN_UNMERGED} deepfire ""

###
### Actual testing
###

## Simple positive baseline
build_should_pass ${WORKING}

## Shadowing
eval_is           "status ${BROKEN_SHADOWED}" shadowed
build_should_fail ${BROKEN_SHADOWED}
nh enable         ${BROKEN_SHADOWED}
nh overrides
build_should_pass ${BROKEN_SHADOWED}

## Unmerged
eval_is           "status ${BROKEN_UNMERGED}" unmerged
build_should_fail ${BROKEN_UNMERGED}
nh enable         ${BROKEN_UNMERGED}
nh overrides
build_should_pass ${BROKEN_UNMERGED}
