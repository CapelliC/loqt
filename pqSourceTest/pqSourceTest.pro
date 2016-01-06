#    pqSourceTest : interfacing SWI-Prolog source files and Qt
#
#    Author       : Carlo Capelli
#    E-mail       : cc.carlo.cap@gmail.com
#    Copyright (C): 2013,2014,2015
#
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

TARGET = pqSourceTest
TEMPLATE = app

unix {
    # because SWI-Prolog is built from source
    CONFIG += link_pkgconfig
    PKGCONFIG += swipl
}

SOURCES += \
    main.cpp

HEADERS +=

RESOURCES +=

OTHER_FILES += \
    screenshots/pqSourceTest.jpg \
    screenshots/eldest.jpg

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../pqSource/release/ -lpqSource
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../pqSource/debug/ -lpqSource
else:unix: LIBS += -L$$OUT_PWD/../pqSource -lpqSource

INCLUDEPATH += $$PWD/../pqSource
DEPENDPATH += $$PWD/../pqSource

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../pqConsole/release/ -lpqConsole
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../pqConsole/debug/ -lpqConsole
else:unix: LIBS += -L$$OUT_PWD/../pqConsole/ -lpqConsole

INCLUDEPATH += $$PWD/../pqConsole
DEPENDPATH += $$PWD/../pqConsole

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../lqUty/release/ -llqUty
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../lqUty/debug/ -llqUty
else:unix:!symbian: LIBS += -L$$OUT_PWD/../lqUty/ -llqUty

INCLUDEPATH += $$PWD/../lqUty
DEPENDPATH += $$PWD/../lqUty

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../pqGraphviz/release/ -lpqGraphviz
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../pqGraphviz/debug/ -lpqGraphviz
else:unix:!symbian: LIBS += -L$$OUT_PWD/../pqGraphviz/ -lpqGraphviz

INCLUDEPATH += $$PWD/../pqGraphviz
DEPENDPATH += $$PWD/../pqGraphviz

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../lqXDot/release/ -llqXDot
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../lqXDot/debug/ -llqXDot
else:unix:!symbian: LIBS += -L$$OUT_PWD/../lqXDot/ -llqXDot

INCLUDEPATH += $$PWD/../lqXDot
DEPENDPATH += $$PWD/../lqXDot

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../pqXml/release/ -lpqXml
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../pqXml/debug/ -lpqXml
else:unix: LIBS += -L$$OUT_PWD/../pqXml/ -lpqXml

INCLUDEPATH += $$PWD/../pqXml
DEPENDPATH += $$PWD/../pqXml
