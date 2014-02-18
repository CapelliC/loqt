#    pqSource     : interfacing SWI-Prolog source files and Qt
#
#    Author       : Carlo Capelli
#    E-mail       : cc.carlo.cap@gmail.com
#    Copyright (C): 2013,2014 Carlo Capelli

#    This library is free software; you can redistribute it and/or
#    modify it under the terms of the GNU Lesser General Public
#    License as published by the Free Software Foundation; either
#    version 2.1 of the License, or (at your option) any later version.

#    This library is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    Lesser General Public License for more details.

#    You should have received a copy of the GNU Lesser General Public
#    License along with this library; if not, write to the Free Software
#    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets
QT += webkit

TARGET = pqSource
TEMPLATE = lib

DEFINES += PQSOURCE_LIBRARY

# please, not obsolete compiler
QMAKE_CXXFLAGS += -std=c++0x

SOURCES += \
    pqSource.cpp \
    pqHighlighter.cpp \
    MdiHelper.cpp \
    pqTrace.cpp \
    pqTextAttributes.cpp \
    pqSourceDebug.cpp \
    pqSyntaxData.cpp \
    symclass.cpp \
    pqSourceMainWindow.cpp \
    MdiChildWithCheck.cpp

HEADERS += \
    pqSource.h \
    pqSource_global.h \
    pqHighlighter.h \
    MdiHelper.h \
    pqSourceMainWindow.h \
    pqTrace.h \
    pqTextAttributes.h \
    pqSyntaxData.h \
    symclass.h \
    MdiChildWithCheck.h

unix {

    # because SWI-Prolog is built from source
    CONFIG += link_pkgconfig
    PKGCONFIG += swipl

    # Graphviz display of structured graphs
    DEFINES += WITH_CGRAPH
    PKGCONFIG += libcgraph libgvc

    # qwt installed from source
    QWT_ROOT = /usr/local/qwt-6.1.0
    include ( $$QWT_ROOT/features/qwt.prf )

    maemo5 {
        target.path = /opt/usr/lib
    } else {
        target.path = /usr/lib
    }
    INSTALLS += target
}

windows {
    SwiPl = "C:\Program Files\pl"
    INCLUDEPATH += $$SwiPl\include
    LIBS += -L$$SwiPl\bin -lswipl
}

RESOURCES += \
    pqSource.qrc

OTHER_FILES += \
    images/save.png \
    images/paste.png \
    images/open.png \
    images/new.png \
    images/cut.png \
    images/copy.png \
    images/undo.png \
    images/print.png \
    prolog/syncol.pl \
    prolog/about_box.pl \
    images/edit-find-and-replace.png \
    images/edit-find-3.png \
    images/zoom-3.png \
    images/run.png \
    images/process-stop-6.png \
    images/media-record-2.png \
    images/edit-undo-8.png \
    images/edit-redo-8.png \
    images/debug-step-over.png \
    images/debug-step-out.png \
    images/debug-step-into-instruction.png \
    images/debug-step-into.png \
    images/debug-step-instruction.png \
    ../../../.config/SWI-Prolog/pqConsole.conf \
    prolog/res/pllisting.css \
    images/wmaker_apps.png \
    prolog/pldoc/h2-bg.png \
    prolog/pldoc/h1-bg.png \
    prolog/pldoc/res/pllisting.css \
    prolog/pldoc/res/pldoc.css \
    prolog/pldoc/source.png \
    prolog/pldoc/public.png \
    prolog/pldoc/pub-bg.png \
    prolog/pldoc/priv-bg.png \
    prolog/pldoc/private.png \
    prolog/pldoc/multi-bg.png \
    prolog/pqSourceTemplate.pl \
    prolog/calledgraph.pl

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../lqUty/release/ -llqUty
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../lqUty/debug/ -llqUty
else:unix:!symbian: LIBS += -L$$OUT_PWD/../lqUty/ -llqUty

INCLUDEPATH += $$PWD/../lqUty
DEPENDPATH += $$PWD/../lqUty

# include Prolog console
win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../pqConsole/release/ -lpqConsole
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../pqConsole/debug/ -lpqConsole
else:unix: LIBS += -L$$OUT_PWD/../pqConsole/ -lpqConsole

INCLUDEPATH += $$PWD/../pqConsole
DEPENDPATH += $$PWD/../pqConsole

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../pqGraphviz/release/ -lpqGraphviz
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../pqGraphviz/debug/ -lpqGraphviz
else:unix:!symbian: LIBS += -L$$OUT_PWD/../pqGraphviz/ -lpqGraphviz

INCLUDEPATH += $$PWD/../pqGraphviz
DEPENDPATH += $$PWD/../pqGraphviz

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../lqXDot/release/ -llqXDot
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../lqXDot/debug/ -llqXDot
else:symbian: LIBS += -llqXDot
else:unix: LIBS += -L$$OUT_PWD/../lqXDot/ -llqXDot

INCLUDEPATH += $$PWD/../lqXDot
DEPENDPATH += $$PWD/../lqXDot
