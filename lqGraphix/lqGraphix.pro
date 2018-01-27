#--------------------------------------------------
# lqGraphix    : SWI-Prolog and Qt Graphics Framework
#--------------------------------------------------
# Author       : Carlo Capelli
# Copyright (C): 2013,2014,2015,2016,2017,2018

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

QT += core gui widgets

TARGET = lqGraphix
TEMPLATE = app

CONFIG += C++11

unix {
    # if SWI-Prolog is built from source
    CONFIG += link_pkgconfig
    PKGCONFIG += swipl
}

SOURCES += \
    main.cpp \
    mainwindow.cpp

HEADERS += \
    mainwindow.h

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

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../lqShapes/release/ -llqShapes
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../lqShapes/debug/ -llqShapes
else:unix: LIBS += -L$$OUT_PWD/../lqShapes/ -llqShapes

INCLUDEPATH += $$PWD/../lqShapes
DEPENDPATH += $$PWD/../lqShapes
