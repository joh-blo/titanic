include ../vsn.mk

# ----------------------------------------------------
# VARIABLES
# ----------------------------------------------------

APP_NAME = titanic

MODULES= \
	titanic \
	titanic_tpd \
	titanic_xmerl \
	titanic_xmerl_xml

TARGET_FILES = $(APP_TARGET) $(EMAKE_TARGET)

include ../../meadow/priv/Makefile.vars

# ----------------------------------------------------
# BUILD TARGETS
# ----------------------------------------------------
all: $(TARGET_FILES)
	erl -make

clean:
	rm -f $(APP_TARGET) $(EMAKE_TARGET) ../ebin/*.beam

doc:

include ../../meadow/priv/Makefile.rules

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------
