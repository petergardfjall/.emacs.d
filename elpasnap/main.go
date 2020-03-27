package main

import (
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	stdexec "os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"github.com/pkg/errors"
	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
)

const (
	Description string = `Takes a snapshot of a git version controlled Emacs package.el package directory
(by default found in ~/.emacs.d/elpa). The snapshot is produced on a separate
branch, which can be checked out and inspected by the user prior to merging with
master and pushing to the remote repo. Each package modification is registered
on the change branch as a separate commit. The program identifies and
distinguishes different types of modifications: additions, deletions, upgrades.
For example:

  new: rainbow-mode-1.0.3
  deleted: which-key-3.4.0
  go-mode-20200309.303 -> go-mode-20200331.407
`
)

var (
	DefaultPackageDir  = filepath.Join(os.Getenv("HOME"), ".emacs.d", "elpa")
	DefaultPullUpdates = true
)

var (
	// packageDir is the targeted Emacs package directory.
	packageDir string
	// pullUpdates is true if we are to pull remote changes before recording
	// local changes as commits.
	pullUpdates bool
)

func init() {
	log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stdout, TimeFormat: time.RFC3339})

	flag.StringVar(&packageDir, "package-dir", DefaultPackageDir, "The emacs package.el installation directory.")
	flag.BoolVar(&pullUpdates, "pull-updates", DefaultPullUpdates, "If true, pull remote changes before recordinglocal changes as commits.")
}

// GitStatus represents an output line from 'git status --porcelain'.
type GitStatus struct {
	State string `json:"State"`
	Path  string `json:"Path"`
	IsDir bool   `json:"IsDir"`
}

func (s GitStatus) String() string {
	data, err := json.Marshal(s)
	if err != nil {
		return fmt.Sprintf("%#v", s)
	}
	return string(data)
}

var (
	statusRegexp = regexp.MustCompile(`^\s*(\S+)\s+(.*)$`)
)

func GitStatusFromString(gitStatus string) (GitStatus, error) {
	if !statusRegexp.MatchString(gitStatus) {
		return GitStatus{}, errors.Errorf("unparsable git status: '%s'", gitStatus)
	}
	m := statusRegexp.FindStringSubmatch(gitStatus)
	state := m[1]
	path := m[2]
	isDir := strings.HasSuffix(path, "/")

	return GitStatus{
		State: state,
		Path:  path,
		IsDir: isDir,
	}, nil
}

func (s GitStatus) IsRootDir() bool {
	return s.IsDir && filepath.Dir(s.Path) == "."
}

// PkgName represents (unversioned) package names such as "undo-tree".
type PkgName string

// Represents an Emacs package in the package directory.
type Pkg struct {
	// path is the full fileName, such as "undo-tree-0.7.4"
	DirName string `json:"dirName"`
	// name is the package name, such as "undo-tree"
	Name PkgName `json:"name"`
	// version is the package version, such as "0.7.4"
	Version string `json:"version"`
}

type PkgUpdate struct {
	Pkg

	// Added is true for a package that has been added since the last
	// master commit.
	Added bool `json:"added,omitempty"`
	// Deleted is true for a package that has been deleted since the last
	// master commit.
	Deleted bool `json:"deleted,omitempty"`
}

func PkgFromDirName(dirName string) (Pkg, error) {
	lastDashIndex := strings.LastIndex(dirName, "-")
	if lastDashIndex < 0 {
		return Pkg{}, errors.Errorf("invalid emacs package (no version field): '%s'", dirName)
	}

	return Pkg{
		DirName: dirName,
		Name:    PkgName(dirName[:lastDashIndex]),
		Version: dirName[lastDashIndex+1:],
	}, nil
}

func NewPkgUpdate(p Pkg) *PkgUpdate {
	return &PkgUpdate{Pkg: p}
}

func (p *PkgUpdate) WithStatus(s GitStatus) *PkgUpdate {
	switch s.State {
	case "??":
		p.Added = true
	case "D":
		p.Deleted = true
	}

	return p
}

func (p Pkg) String() string {
	data, err := json.Marshal(p)
	if err != nil {
		return fmt.Sprintf("%#v", p)
	}
	return string(data)
}

// determines the existing (version-controlled) list of packages in the package
// directory, by finding all root-level directories that are valid package
// names.
func listExistingPackages() (map[PkgName]Pkg, error) {
	pkgs := make(map[PkgName]Pkg)

	trackedPaths, err := exec("git", "ls-tree", "-r", "--name-only", "HEAD")
	if err != nil {
		log.Fatal().Msgf(err.Error())
	}

	// only care for root-level directories with valid package names
	for _, path := range trackedPaths {
		if !hasRootDir(path) {
			continue
		}
		pkg, err := PkgFromDirName(rootDir(path))
		if err != nil {
			continue
		}
		pkgs[PkgName(pkg.Name)] = pkg
	}

	return pkgs, nil
}

// returns true if the path has a top-level directory, such as
// 'swiper-0.13.0/swiper.el', 'abc/xy/z.el', but not 'README.md'.
func hasRootDir(path string) bool {
	path = filepath.ToSlash(path)
	return strings.Contains(path, "/")
}

// returns the root path of given path, such as 'swiper-0.13.0' for
// 'swiper-0.13.0/swiper.el' and 'abc' for 'abc/xy/z.el'.
func rootDir(path string) string {
	pathParts := strings.Split(path, "/")
	return pathParts[0]
}

// TODO: skips empty stdout lines
func exec(command string, args ...string) (stdoutLines []string, err error) {
	var stdoutBuf, stderrBuf bytes.Buffer
	cmd := stdexec.Command(command, args...)
	cmd.Stdout = &stdoutBuf
	cmd.Stderr = &stderrBuf

	log.Debug().Msgf("exec: %s %s", command, strings.Join(args, " "))

	if err := cmd.Run(); err != nil {
		return nil, errors.Errorf("failed to run '%s': %v\n%s", cmd, err, stderrBuf.Bytes())
	}

	lines := strings.Split(stdoutBuf.String(), "\n")
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		stdoutLines = append(stdoutLines, line)
	}

	return
}

func execOrDie(command string, args ...string) (stdoutLines []string) {
	stdout, err := exec(command, args...)
	if err != nil {
		log.Fatal().Msgf(err.Error())
	}
	return stdout
}

func added(updates []*PkgUpdate) []*PkgUpdate {
	var added []*PkgUpdate
	for _, pkg := range updates {
		if pkg.Added {
			added = append(added, pkg)
		}
	}
	return added
}

func deleted(updates []*PkgUpdate) []*PkgUpdate {
	var deleted []*PkgUpdate
	for _, pkg := range updates {
		if pkg.Deleted {
			deleted = append(deleted, pkg)
		}
	}
	return deleted
}

func filterOut(updates []*PkgUpdate, name PkgName) []*PkgUpdate {
	var remaining []*PkgUpdate
	for _, u := range updates {
		if u.Name != name {
			remaining = append(remaining, u)
		}
	}
	return remaining
}

// TODO: what if pkg remove is listed before pkg add
// find all Emacs packages in the package directory that have changes as
// compared to the current commit.
func getPkgUpdates() (updates []*PkgUpdate) {
	encounteredPkgDirs := make(map[string]bool)

	gitStatusLines := execOrDie("git", "status", "--porcelain")
	for _, statusLine := range gitStatusLines {
		status, err := GitStatusFromString(statusLine)
		if err != nil {
			log.Fatal().Msgf(err.Error())
		}
		if !hasRootDir(status.Path) {
			log.Debug().Msgf("ignoring non-root directory %s", status.Path)
			continue
		}

		// we only care about the package root dir
		pkg, err := PkgFromDirName(rootDir(status.Path))
		if err != nil {
			log.Debug().Msgf("ignoring non-package directory for status %s: %v", status, err)
			continue
		}

		// note: it is assumed that a single package is either
		// completely addded or removed by package.el. Situations such
		// as this is assumed to never occur:
		//
		//    added:      swiper-0.13.0/swiper-autoloads.el
		//    deleted:    swiper-0.13.0/swiper-pkg.el
		//    added:      swiper-0.13.0/swiper.el
		//
		if _, ok := encounteredPkgDirs[pkg.DirName]; ok {
			continue
		}
		updates = append(updates, NewPkgUpdate(pkg).WithStatus(status))
		encounteredPkgDirs[pkg.DirName] = true
	}

	return updates
}

// pull any upstream changes to master and apply any local changes on top of
// that.
func pullRemoteChangesAndRebase() {
	execOrDie("git", "fetch", "origin", "master")
	if stdout := execOrDie("git", "rev-list", "--count", "master..origin/master"); stdout[0] == "0" {
		log.Info().Msgf("no remote changes.")
	} else {
		log.Info().Msgf("pulling remote changes to master ...")
		// unstage and stash any local changes temporarily before the
		// rebase begins and apply it again after the rebase ends (we
		// want to apply them on a change branch)
		execOrDie("git", "rebase", "--autostash", "origin/master")
	}
}

func main() {
	flag.Usage = func() {
		fmt.Fprintf(flag.CommandLine.Output(), fmt.Sprintf("%s\n\nOptions:\n", Description))
		flag.PrintDefaults()
	}
	flag.Parse()

	if err := os.Chdir(packageDir); err != nil {
		log.Fatal().Msgf("failed to enter package directory %s: %v", packageDir, err)
	}

	execOrDie("git", "checkout", "master")

	if pullUpdates {
		pullRemoteChangesAndRebase()
	}

	existingPkgs, err := listExistingPackages()
	if err != nil {
		log.Fatal().Msgf("failed to list existing package dirs: %v", err)
	}

	pkgUpdates := getPkgUpdates()
	numUpdates := len(pkgUpdates)
	if numUpdates == 0 {
		log.Info().Msgf("no local changes.")
		os.Exit(0)
	}

	//
	// apply changes to a local branch
	//
	localBranch, err := os.Hostname()
	if err != nil {
		log.Fatal().Msgf("failed to use hostname as branch name: %v", err)
	}

	execOrDie("git", "checkout", "-B", localBranch, "origin/master")

	// record added packages
	for _, pkg := range added(pkgUpdates) {
		var commitMsg string
		existing, ok := existingPkgs[pkg.Name]
		if ok {
			// if an earlier version of the package existed: upgrade
			execOrDie("git", "rm", "-rf", existing.DirName)
			commitMsg += existing.DirName + " -> "
			// if also among deleted packages, we can ignore that
			pkgUpdates = filterOut(pkgUpdates, pkg.Name)
		} else {
			commitMsg += "new: "
		}
		execOrDie("git", "add", pkg.DirName)
		commitMsg += pkg.DirName
		execOrDie("git", "commit", "-m", commitMsg)
	}

	// record removed packages
	for _, pkg := range deleted(pkgUpdates) {
		execOrDie("git", "rm", "-rf", "--ignore-unmatch", pkg.DirName)
		execOrDie("git", "commit", "-m", fmt.Sprintf("deleted: %s", pkg.DirName))
	}

	log.Info().Msgf("%d package(s) changed:\n%s", numUpdates, strings.Join(execOrDie("git", "log", "--oneline", "master.."+localBranch), "\n"))
	log.Info().Msgf("To apply changes:\n\ncd %s && git checkout master && git merge %s", packageDir, localBranch)
}
