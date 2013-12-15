# See LICENSE for licensing information.

PROJECT = ext_cowboy

# Options.

COMPILE_FIRST = ext_multipart_handler
CT_SUITES = http
PLT_APPS = crypto public_key ssl

# Dependencies.

DEPS = cowboy re_utils
dep_cowboy = pkg://cowboy 0.9.0

TEST_DEPS = ct_helper gun re_utils
dep_ct_helper = https://github.com/extend/ct_helper.git master
dep_gun = pkg://gun master
dep_re_utils = https://github.com/egobrain/re_utils.git 0.1.0

# Standard targets.

include erlang.mk
