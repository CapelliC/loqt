#--------------------------------------------------
# pqConsole.pro: SWI-Prolog / QT interface
#--------------------------------------------------
# REPL in QTextEdit on a background logic processor
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

TARGET = pqConsole
TEMPLATE = lib

DEFINES += PQCONSOLE_LIBRARY

# moved where the class is defined
# DEFINES += PQCONSOLE_BROWSER

CONFIG += C++11

SOURCES += \
    pqConsole.cpp \
    SwiPrologEngine.cpp \
    ConsoleEdit.cpp \
    pqTerm.cpp \
    Completion.cpp \
    Swipl_IO.cpp \
    pqMainWindow.cpp \
    Preferences.cpp \
    FlushOutputEvents.cpp \
    pqApplication.cpp \
    win_builtins.cpp \
    pqMiniSyntax.cpp \
    pqMeta.cpp \
    pqProofScene.cpp \
    pqProofView.cpp \
    pqProof.cpp

HEADERS += \
    pqConsole.h \
    pqConsole_global.h \
    SwiPrologEngine.h \
    ConsoleEdit.h \
    PREDICATE.h \
    pqTerm.h \
    Completion.h \
    Swipl_IO.h \
    pqMainWindow.h \
    Preferences.h \
    FlushOutputEvents.h \
    pqApplication.h \
    pqMiniSyntax.h \
    pqMeta.h \
    pqProofScene.h \
    pqProofView.h \
    pqProof.h \
    swi.h

unix: {
    # because SWI-Prolog is built from source
    CONFIG += link_pkgconfig
    PKGCONFIG += swipl

    maemo5 {
        target.path = /opt/usr/lib
    } else {
        target.path = /usr/lib
    }

    INSTALLS += target
}

OTHER_FILES += \
    README.md \
    pqConsole.doxy \
    swipl.png \
    trace_interception.pl \
    win_html_write_help.pl

RESOURCES += \
    pqConsole.qrc

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../lqUty/release/ -llqUty
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../lqUty/debug/ -llqUty
else:unix: LIBS += -L$$OUT_PWD/../lqUty/ -llqUty

INCLUDEPATH += $$PWD/../lqUty
DEPENDPATH += $$PWD/../lqUty
