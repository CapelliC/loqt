#--------------------------------------------------
# pqGraphviz.pro: SWI-Prolog / Graphviz / Qt
#--------------------------------------------------
# interface SWI-Prolog and lqXDot
#--------------------------------------------------
# Copyright (C) : 2013,2014 Carlo Capelli

QT += gui widgets webkit webkitwidgets concurrent

TARGET = pqGraphviz
TEMPLATE = lib

DEFINES += PQGRAPHVIZ_LIBRARY

SOURCES += \
    pqGraphviz.cpp \
    pqDocView.cpp

HEADERS += \
    pqGraphviz.h \
    pqGraphviz_global.h \
    pqDocView.h

OTHER_FILES += \
    prolog/gv_uty.pl \
    test/genealogy/genealogy.pl \
    test/genealogy/familiari.pl \
    test/odbc/odbc_schema.pl \
    test/xref/paths_prefix.pl \
    test/xref/file_xref.pl \
    test/odbc/graph_schema.pl \
    prolog/termtree.pl \
    test/genealogy/familiari.pdf \
    test/genealogy/pqGraphviz_emu.pl \
    test/genealogy/allocator.pl

# use some modern features
QMAKE_CXXFLAGS += -std=c++0x

unix {
    target.path = /usr/lib
    INSTALLS += target
}

unix:!macx {
    # because SWI-Prolog is built from source
    CONFIG += link_pkgconfig

    PKGCONFIG += swipl

    DEFINES += WITH_CGRAPH

    # latest download (2013/11/29) refuses to compile without this useless define
    DEFINES += HAVE_STRING_H

    PKGCONFIG += libcgraph libgvc
}

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../lqXDot/release/ -llqXDot
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../lqXDot/debug/ -llqXDot
else:unix: LIBS += -L$$OUT_PWD/../lqXDot/ -llqXDot

INCLUDEPATH += $$PWD/../lqXDot
DEPENDPATH += $$PWD/../lqXDot

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../pqConsole/release/ -lpqConsole
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../pqConsole/debug/ -lpqConsole
else:unix:!symbian: LIBS += -L$$OUT_PWD/../pqConsole/ -lpqConsole

INCLUDEPATH += $$PWD/../pqConsole
DEPENDPATH += $$PWD/../pqConsole

RESOURCES += \
    pqGraphviz.qrc

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../lqUty/release/ -llqUty
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../lqUty/debug/ -llqUty
else:unix:!symbian: LIBS += -L$$OUT_PWD/../lqUty/ -llqUty

INCLUDEPATH += $$PWD/../lqUty
DEPENDPATH += $$PWD/../lqUty
