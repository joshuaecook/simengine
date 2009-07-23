# Top-level Makefile
# Copyright 2007, Simatra Modeling Technologies
.SILENT:

SVNSRV = https://svn1.hosted-projects.com/simatra/simEngine
TRUNK = $(SVNSRV)/trunk
TAGS = $(SVNSRV)/tags
BRANCHES = $(SVNSRV)/branches
BUILD = $(SVNSRV)/build

RM = rm -rf

help:
	@echo " "
	@echo "**********************************************************"
	@echo "****       Simatra Dynamo Development Environment     ****"
	@echo "**********************************************************"
	@echo "****                                                  ****"
	@echo "****  make help - display this message                ****"
	@echo "****                                                  ****"
	@echo "****  make setup - generates all directories and      ****"
	@echo "****               checks out the trunk               ****"
	@echo "****                                                  ****"
	@echo "****  make list-branches - available branches         ****"
	@echo "****  make list-tags - available tags                 ****"
	@echo "****  make list-builds - available builds             ****"
	@echo "****                                                  ****"
	@echo "****  make co-branch - checkout a branch              ****"
	@echo "****  make co-tag - checkout a tag                    ****"
	@echo "****  make co-build - checkout a build                ****"
	@echo "****                                                  ****"
	@echo "****  make create-branch - creates a new branch from  ****"
	@echo "****                       from the trunk             ****"
	@echo "****                                                  ****"
	@echo "****  make create-tag - creates a new tag from from   ****"
	@echo "****                    from the trunk                ****"
	@echo "****                                                  ****"
	@echo "****  make co-rev - checkout a revision of the trunk  ****"
	@echo "****                                                  ****"
	@echo "****  make merge-to-trunk - dry-run merge to trunk    ****"
	@echo "****                                                  ****"
	@echo "****  make clean-all - be careful!!!!                 ****"
	@echo "****                                                  ****"
	@echo "**********************************************************"
	@echo " "

UNAME = $(shell uname -s)
ifeq ($(UNAME), CYGWIN_NT-5.1)
PLATFORM=cyg
else
ifeq ($(UNAME), Darwin)
PLATFORM=mac
else
ifeq ($(UNAME), Linux)
PLATFORM=lin
else
PLATFORM=unknown
endif
endif
endif

tags:
	mkdir tags

trunk:
	@svn checkout $(TRUNK)

branches:
	mkdir branches

build:
	mkdir build
#	svn checkout -N $(BUILD)
#	svn update -N $(BUILD)/$(PLATFORM)
#	@(cd build; svn checkout -N $(BUILD)/$(PLATFORM))

setup: trunk tags branches build

list-branches:
	@echo "    Rev User                  Date         Branch"
	@echo "----------------------------------------------------------"
	@svn list -v $(BRANCHES) | sort -n
	@echo " "

list-tags:
	@echo "    Rev User                  Date         Tag"
	@echo "----------------------------------------------------------"
	@svn list -v $(TAGS) | sort -n
	@echo " "

list-builds:
	@echo "Retrieving list of builds on server ..."
	@svn list -v $(BUILD)/$(PLATFORM) | sort -n > build_list
	@-grep _release_ build_list > tag_list
	@-grep _devbuild_ build_list > trunk_list
	@-grep _branch_ build_list > branch_list
	@echo " "
	@if [ -s tag_list ] ; then \
	 echo " Release Builds" ;\
	 echo "    Rev User                  Date         Tag" ;\
	 echo "----------------------------------------------------------" ;\
	 grep _release_ build_list; \
	 echo " "; \
	fi
	@if [ -s trunk_list ] ; then \
	 echo " Development Builds" ;\
	 echo "    Rev User                  Date         Tag" ;\
	 echo "----------------------------------------------------------" ;\
	 grep _devbuild_ build_list; \
	 echo " "; \
	fi
	@if [ -s branch_list ] ; then \
	 echo " Experimental Builds" ;\
	 echo "    Rev User                  Date         Tag" ;\
	 echo "----------------------------------------------------------" ;\
	 grep _branch_ build_list; \
	 echo " "; \
	fi
	@$(RM) build_list trunk_list tag_list branch_list

co-branch: branches
	@make list-branches
	@echo "Enter branch to pull from subversion: "
	@read line; \
	 echo "Checking out <$$line> ..."; \
	 svn checkout $(BRANCHES)/$$line branches/$$line

co-tag: tags
	@make list-tags
	@echo "Enter tag to pull from subversion: "
	@read line; \
	 echo "Checking out <$$line> ..."; \
	 svn checkout $(TAGS)/$$line tags/$$line

co-build: build
	@make list-builds
	@echo "Enter name of build to pull from subversion: "
	@read line; \
	 echo "Checking out <$$line> ..."; \
	 svn cat $(BUILD)/$(PLATFORM)/$$line > build/$$line ; \
	 echo "done"

co-rev: trunk
	@svn log trunk
	@echo " "
	@echo "Enter the revision number that you would like to check out: "
	@read line; \
	 echo "Checking out revision r$$line ..."; \
	 svn checkout -r$$line $(TRUNK) trunk_r$$line; \
	 echo "done"

create-branch:
	@echo "Enter a branch name, usually in the form of <username>_<revision>:"; \
	 read branchname; \
         echo "Creating $$branchname ..."; \
         svn copy $(TRUNK) $(BRANCHES)/$$branchname -m "Creating branch: $$branchname"; \
         svn checkout $(BRANCHES)/$$branchname branches/$$branchname;# \
#	 (cd trunk; svnmerge.py init ../branches/$$branchname; \
#	  svn commit -F svnmerge-commit-message.txt; \
#	  rm svnmerge-commit-message.txt); \
#	 (cd branches/$$branchname; svnmerge.py init ../trunk; \
#	  svn commit -F svnmerge-commit-message.txt; \
#	  rm svnmerge-commit-message.txt)

create-tag:
	@echo "Enter the tag name which will grab the latest version of the trunk:"; \
	 read tagname; \
	 echo "Creating $$tagname ..."; \
	 svn copy $(TRUNK) $(TAGS)/$$tagname -m "Creating tag: $$tagname"; \
	 svn checkout $(TAGS)/$$tagname tags/$$tagname

merge-to-trunk-prev:
	@make list-branches; \
	 echo "Enter a branch name, usually in the form of <username>_<revision>:"; \
	 read branchname; \
         echo "Evaluating merge of $$branchname back to trunk..."; \
	 cd branches/$$branchname; \
	 startrev=`svn log --stop-on-copy | egrep '^r[0-9]+ \|' | tail -1 | awk '{print $$1}' | sed -e 's/r//'`; \
	 cd ../..; \
	 echo " -> Branch was initally created in revision $$startrev"; \
	 echo " -> Creating trunk as of revision $$startrev in branch merge_trunk_$$branchname"; \
	 $(RM) merge_trunk_$$branchname; \
	 svn checkout -r$$startrev $(TRUNK) merge_trunk_$$branchname; \
	 cd merge_trunk_$$branchname; \
	 echo " -> Updating working merge branch to current revision"; \
	 svn update; \
	 startrev_plus_one=`echo $$startrev | awk '{print $$1 + 1}'`; \
	 svn merge -r$$startrev:HEAD $(BRANCHES)/$$branchname; \
	 echo " -> Resolve any conflicts marked with a 'C'"; \
	 cd ..; \
	 echo " -> Execute 'svn commit -m \"Merged $$branchname back to the trunk\"' in directory 'merge_trunk_$$branchname'"
#         svn --dry-run merge $(TRUNK) $(BRANCHES)/$$branchname trunk; \
	 echo "To merge back to the trunk, run the following command:"; \
	 echo " svn merge $(TRUNK) $(BRANCHES)/$$branchname trunk"; \
	 echo "resolve conflicts and then follow up with a commit."


merge-to-trunk:
	@make list-branches; \
	 echo "Enter a branch name, usually in the form of <username>_<revision>:"; \
	 read branchname; \
         echo "Evaluating merge of $$branchname back to the trunk..."; \
	 echo "First, creating directory merge_trunk_$$branchname with the latest copy of the trunk"; \
	 $(RM) merge_trunk_$$branchname ; \
	 svn checkout $(TRUNK) merge_trunk_$$branchname ; \
	 echo "Now, merging the branch into this directory"; \
	 cd merge_trunk_$$branchname; \
	 svn merge $(BRANCHES)/$$branchname ; \
	 echo "Correct all the conflicts in this directory and then"; \
	 echo " -> Execute 'svn commit -m \"Merged $$branchname back to the trunk\"' in directory 'merge_trunk_$$branchname'"


merge-to-trunk-old:
	@make list-branches; \
	 echo "Enter a branch name, usually in the form of <username>_<revision>:"; \
	 read branchname; \
         echo "Evaluating merge of $$branchname back to trunk..."; \
	 cd branches/$$branchname; \
#	 startrev=`svn log --stop-on-copy | egrep '^r[0-9]+ \|' | tail -1 | awk '{print $$1}' | sed -e 's/r//'`; \
	 cd ../..; \
	 echo " -> Branch was initally created in revision $$startrev"; \
#	 echo " -> Creating trunk as of revision $$startrev in branch merge_trunk_$$branchname"; \
	 echo " -> Creating copy of the trunk in branch merge_trunk_$$branchname"; \
	 $(RM) merge_trunk_$$branchname; \
#	 svn checkout -r$$startrev $(TRUNK) merge_trunk_$$branchname; \
	 svn checkout -rHEAD $(TRUNK) merge_trunk_$$branchname; \
	 cd merge_trunk_$$branchname; \
#	 echo " -> Updating working merge branch to current revision"; \
#	 svn update; \
	 startrev_plus_one=`echo $$startrev | awk '{print $$1 + 1}'`; \
	 svn merge -r$$startrev:HEAD $(BRANCHES)/$$branchname; \
	 echo " -> Resolve any conflicts marked with a 'C'"; \
	 cd ..; \
	 echo " -> Execute 'svn commit -m \"Merged $$branchname back to the trunk\"' in directory 'merge_trunk_$$branchname'"
#         svn --dry-run merge $(TRUNK) $(BRANCHES)/$$branchname trunk; \
	 echo "To merge back to the trunk, run the following command:"; \
	 echo " svn merge $(TRUNK) $(BRANCHES)/$$branchname trunk"; \
	 echo "resolve conflicts and then follow up with a commit."

clean-all:
	@rm -rf tags trunk branches

