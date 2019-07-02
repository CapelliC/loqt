#    pqSource_test: interfacing SWI-Prolog source files and Qt
#
#    Author       : Carlo Capelli
#    E-mail       : cc.carlo.cap@gmail.com
#    Copyright (C): 2013,2014,2015,2016,2017,2018,2019
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

INCLUDEPATH += $$PWD/../pqSource
DEPENDPATH += $$PWD/../pqSource
LIBS += -L$$OUT_PWD/../pqSource -lpqSource

INCLUDEPATH += $$PWD/../pqConsole
DEPENDPATH += $$PWD/../pqConsole
LIBS += -L$$OUT_PWD/../pqConsole/ -lpqConsole

INCLUDEPATH += $$PWD/../lqUty
DEPENDPATH += $$PWD/../lqUty
LIBS += -L$$OUT_PWD/../lqUty/ -llqUty

INCLUDEPATH += $$PWD/../pqGraphviz
DEPENDPATH += $$PWD/../pqGraphviz
LIBS += -L$$OUT_PWD/../pqGraphviz/ -lpqGraphviz

INCLUDEPATH += $$PWD/../lqXDot
DEPENDPATH += $$PWD/../lqXDot
LIBS += -L$$OUT_PWD/../lqXDot/ -llqXDot

INCLUDEPATH += $$PWD/../pqXml
DEPENDPATH += $$PWD/../pqXml
LIBS += -L$$OUT_PWD/../pqXml/ -lpqXml
