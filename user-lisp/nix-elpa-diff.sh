#!/usr/bin/env bash
# Diff Emacs Lisp packages between two Nix system generations.
#
# Usage: diff-emacs-pkgs [OLD_GENERATION NEW_GENERATION]
#
# With no arguments, diffs the previous system generation against the
# current one. Each generation argument may be given as:
#   - a generation id: 42            (shorthand for system-42-link)
#   - a profile name:  system-42-link (relative to /nix/var/nix/profiles/)
#   - an absolute path to a generation
#
# Added/removed package lists go to stderr; stdout carries only diffs,
# so you can pipe it into a pager. Exit status is 1 if any package differs.

set -uo pipefail
export LC_ALL=C   # deterministic sorting and glob expansion

die() { printf 'error: %s\n' "$*" >&2; exit 2; }

profiles_dir=/nix/var/nix/profiles

# ---------------------------------------------------------------------------
# Generation resolution helpers
# ---------------------------------------------------------------------------

# Resolve a generation argument to a path:
#   42             -> /nix/var/nix/profiles/system-42-link
#   system-42-link -> /nix/var/nix/profiles/system-42-link
#   /abs/path      -> /abs/path
resolve_generation() {
    case $1 in
        /*)          printf '%s\n' "$1" ;;
        ''|*[!0-9]*) printf '%s\n' "$profiles_dir/$1" ;;
        *)           printf '%s\n' "$profiles_dir/system-$1-link" ;;
    esac
}

# Extract N from a .../system-N-link path.
generation_number() {
    local n=${1##*/}
    n=${n#system-}
    printf '%s\n' "${n%-link}"
}

# The current generation: whatever the `system` symlink points at.
current_generation() {
    local target
    [ -e "$profiles_dir/system" ] || die "no system profile at $profiles_dir/system"
    target=$(readlink "$profiles_dir/system") \
        || die "cannot read $profiles_dir/system"
    case $target in
        /*) printf '%s\n' "$target" ;;
        *)  printf '%s\n' "$profiles_dir/$target" ;;
    esac
}

# Highest-numbered system generation below N (gaps from deletions are ok).
previous_generation() {
    local cur_n=$1 d n prev_n=
    for d in "$profiles_dir"/system-*-link; do
        [ -e "$d" ] || continue
        n=$(generation_number "$d")
        case $n in ''|*[!0-9]*) continue ;; esac
        if (( n < cur_n )) && { [ -z "$prev_n" ] || (( n > prev_n )); }; then
            prev_n=$n
        fi
    done
    [ -n "$prev_n" ] \
        || die "no generation older than system-$cur_n-link exists"
    printf '%s\n' "$profiles_dir/system-$prev_n-link"
}

# ---------------------------------------------------------------------------
# Emacs site-lisp discovery
# ---------------------------------------------------------------------------

# $gen/sw/bin/emacs is a wrapper script; the real binary lives next to
# .emacs-wrapped, which exports emacsWithPackages_siteLisp.
sitelisp_from_generation() {
    local emacs wrapped line
    emacs=$1/sw/bin/emacs
    [ -e "$emacs" ] || die "generation has no emacs: $1"
    wrapped=$(dirname -- "$(readlink -f -- "$emacs")")/.emacs-wrapped
    [ -f "$wrapped" ] || die "not an emacs-with-packages wrapper: $1"
    line=$(grep '^export emacsWithPackages_siteLisp=' "$wrapped") \
        || die "no siteLisp export in wrapper (too-old nixpkgs?): $1"
    printf '%s\n' "${line#*=}"   # strip up to first '=', keep the rest
}

# Print version-stripped package names under <siteLisp>/elpa, sorted.
list_packages() (
    shopt -s nullglob
    local dir base
    for dir in "$1"/elpa/*; do
        [ -d "$dir" ] || continue
        base=${dir##*/}
        printf '%s\n' "${base%%-[0-9]*}"
    done | sort -u
)

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

case $# in
    0)
        # Default: previous generation -> current generation.
        new=$(current_generation)
        old=$(previous_generation "$(generation_number "$new")")
        ;;
    2)
        old=$(resolve_generation "$1")
        new=$(resolve_generation "$2")
        ;;
    *)
        printf 'usage: %s [OLD_GENERATION NEW_GENERATION]\n' "${0##*/}" >&2
        exit 1
        ;;
esac

for gen in "$old" "$new"; do
    [ -e "$gen" ] || die "generation does not exist: $gen"
done

echo "# -*- mode: diff; -*-"

site1=$(sitelisp_from_generation "$old")
site2=$(sitelisp_from_generation "$new")

mapfile -t removed < <(comm -23 <(list_packages "$site1") <(list_packages "$site2"))
mapfile -t added   < <(comm -13 <(list_packages "$site1") <(list_packages "$site2"))
mapfile -t common  < <(comm -12 <(list_packages "$site1") <(list_packages "$site2"))

if ((${#removed[@]})); then
    printf 'removed packages (%d):\n' "${#removed[@]}" >&2
    printf '  %s\n' "${removed[@]}" >&2
fi
if ((${#added[@]})); then
    printf 'added packages (%d):\n' "${#added[@]}" >&2
    printf '  %s\n' "${added[@]}" >&2
fi

diffs=0
for pkg in "${common[@]}"; do
    for d1 in "$site1/elpa/$pkg"-[0-9]*; do
        [ -d "$d1" ] || continue
        for d2 in "$site2/elpa/$pkg"-[0-9]*; do
            [ -d "$d2" ] || continue
            # cheap -q pre-pass: full diff (and header) only when needed
            if ! diff -qr -x '*.elc' -x "*-pkg.el" -x "*.info" "$d1" "$d2" >/dev/null; then
                printf '\n===== %s =====\n' "$pkg"
                diff -Nur -x '*.elc' -x "*-pkg.el" -x "*.info" \
                     -F "^(" \
                     "$d1" "$d2" || diffs=1
            fi
        done
    done
done

exit "$diffs"
