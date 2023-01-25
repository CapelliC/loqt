#    pqSource_test: interfacing SWI-Prolog source files and Qt
#
#    Author       : Carlo Capelli
#    E-mail       : cc.carlo.cap@gmail.com
#    Copyright @ 2023
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

TARGET = pqSource_test
TEMPLATE = app

# uses lqUty etc
CONFIG += link_prl

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
    screenshots/eldest.jpg

INCLUDEPATH += $$PWD/../lqUty
DEPENDPATH += $$PWD/../lqUty/debug
LIBS += -L$$OUT_PWD/../lqUty/debug -llqUty

INCLUDEPATH += $$PWD/../lqXDot
DEPENDPATH += $$PWD/../lqXDot/debug
LIBS += -L$$OUT_PWD/../lqXDot/debug -llqXDot

INCLUDEPATH += $$PWD/../pqGraphviz
DEPENDPATH += $$PWD/../pqGraphviz/debug
LIBS += -L$$OUT_PWD/../pqGraphviz/debug -lpqGraphviz

INCLUDEPATH += $$PWD/../pqXml
DEPENDPATH += $$PWD/../pqXml/debug
LIBS += -L$$OUT_PWD/../pqXml/debug -lpqXml

INCLUDEPATH += $$PWD/../pqConsole
DEPENDPATH += $$PWD/../pqConsole/debug
LIBS += -L$$OUT_PWD/../pqConsole/debug -lpqConsole

INCLUDEPATH += $$PWD/../pqSource
DEPENDPATH += $$PWD/../pqSource/debug
LIBS += -L$$OUT_PWD/../pqSource/debug -lpqSource
