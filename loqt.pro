#--------------------------------------------------
# loqt.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Author        : Carlo Capelli
# Copyright (C) : 2013,2014,2015

# please, not obsolete compiler
QMAKE_CXXFLAGS += -std=c++0x

QT += core gui webkit widgets

TEMPLATE = subdirs

SUBDIRS += \
    fdqueens \
    lqUty \
    lqXDot \
    lqXDot_test \
    pqConsole \
    pqGraphviz \
    pqSource \
    pqSourceTest \
    spqr \
    qPrologPad \
    testKeyboardMacros

OTHER_FILES += \
    README.md \
    img/cluster.png \
    img/cluster-dot.png

#win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/pqSource/release/ -lpqSource
#else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/pqSource/debug/ -lpqSource
#else:unix: LIBS += -L$$OUT_PWD/pqSource/ -lpqSource

#INCLUDEPATH += $$PWD/lqUty
#DEPENDPATH += $$PWD/lqUty
