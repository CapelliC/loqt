#--------------------------------------------------
# qPrologPad.pro: SWI-Prolog PrologPad in Qt
#--------------------------------------------------
# prologpad courtesy Jan Wielemaker
#--------------------------------------------------
# Copyright (C) : 2014,2015,2016 Carlo Capelli

#    This library is free software; you can redistribute it and/or
#    modify it under the terms of the GNU Lesser General Public
#    License as published by the Free Software Foundation; either
#    version 2.1 of the License, or (at your option) any later version.
#
#    This library is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    Lesser General Public License for more details.
#
#    You should have received a copy of the GNU Lesser General Public
#    License along with this library; if not, write to the Free Software
#    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

include(../loqt.pri)

TARGET = qPrologPad
TEMPLATE = app

SOURCES += \
    main.cpp \
    qppView.cpp \
    qppViewFile.cpp \
    qppMainWindow.cpp

HEADERS += \
    qppView.h \
    qppViewFile.h \
    qppMainWindow.h

unix:!macx {
    # because SWI-Prolog is built from source
    CONFIG += link_pkgconfig
    PKGCONFIG += swipl
}

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../lqXDot/release/ -llqXDot
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../lqXDot/debug/ -llqXDot
else:unix: LIBS += -L$$OUT_PWD/../lqXDot/ -llqXDot

INCLUDEPATH += $$PWD/../lqXDot
DEPENDPATH += $$PWD/../lqXDot

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../lqUty/release/ -llqUty
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../lqUty/debug/ -llqUty
else:unix: LIBS += -L$$OUT_PWD/../lqUty/ -llqUty

INCLUDEPATH += $$PWD/../lqUty
DEPENDPATH += $$PWD/../lqUty

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../pqConsole/release/ -lpqConsole
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../pqConsole/debug/ -lpqConsole
else:unix: LIBS += -L$$OUT_PWD/../pqConsole/ -lpqConsole

INCLUDEPATH += $$PWD/../pqConsole
DEPENDPATH += $$PWD/../pqConsole

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../pqGraphviz/release/ -lpqGraphviz
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../pqGraphviz/debug/ -lpqGraphviz
else:unix: LIBS += -L$$OUT_PWD/../pqGraphviz/ -lpqGraphviz

INCLUDEPATH += $$PWD/../pqGraphviz
DEPENDPATH += $$PWD/../pqGraphviz

RESOURCES += \
    qPrologPad.qrc

OTHER_FILES += \
    prolog/newFileDefaultScript.pl \
    qppView.html \
    prologpad/test.pl \
    prologpad/server.pl \
    prologpad/README.md \
    prologpad/jquery.pl \
    prologpad/jquery.js \
    prologpad/ide.css \
    prologpad/fuelux.pl \
    prologpad/filetree.pl \
    prologpad/codemirror.pl \
    prologpad/codemirror/LICENSE \
    prologpad/codemirror/lib/codemirror.js \
    prologpad/codemirror/lib/codemirror.css \
    prologpad/codemirror/mode/prolog/prolog.js
