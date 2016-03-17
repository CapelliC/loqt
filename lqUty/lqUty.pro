#--------------------------------------------------
# lqUty.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Author        : Carlo Capelli
# Copyright (C) : 2013,2014,2015,2016

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

TARGET = lqUty
TEMPLATE = lib

DEFINES += LQUTY_LIBRARY

SOURCES += \
    lqUty.cpp \
    ParenMatching.cpp \
    MruHelper.cpp \
    RowColIndicators.cpp \
    lqPreferences.cpp \
    QStackedWidget_KeybTabs.cpp \
    FindReplace.cpp \
    lqLogger.cpp \
    lqAniMachine.cpp \
    CodeMirrorFile.cpp \
    CodeMirror.cpp \
    XmlSyntaxHighlighter.cpp \
    KeyboardMacros.cpp \
    JSSyntax.cpp \
    framedTextAttr.cpp \
    foldedTextAttr.cpp \
    foldingQTextEdit.cpp

HEADERS += \
    lqUty.h \
    lqUty_global.h \
    ParenMatching.h \
    do_events.h \
    MruHelper.h \
    RowColIndicators.h \
    lqPreferences.h \
    blockSig.h \
    QStackedWidget_KeybTabs.h \
    file2string.h \
    FindReplace.h \
    lqLogger.h \
    lqAniMachine.h \
    CodeMirrorFile.h \
    CodeMirror.h \
    CenterWidgets.h \
    XmlSyntaxHighlighter.h \
    EditInterface.h \
    thousandsDots.h \
    maplist.h \
    KeyboardMacros.h \
    Tabbler.h \
    JSSyntax.h \
    framedTextAttr.h \
    foldedTextAttr.h \
    foldingQTextEdit.h

symbian {
    MMP_RULES += EXPORTUNFROZEN
    TARGET.UID3 = 0xE3BE79CA
    TARGET.CAPABILITY =
    TARGET.EPOCALLOWDLLDATA = 1
    addFiles.sources = lqUty.dll
    addFiles.path = !:/sys/bin
    DEPLOYMENT += addFiles
}

unix:!symbian {
    maemo5 {
        target.path = /opt/usr/lib
    } else {
        target.path = /usr/lib
    }
    INSTALLS += target
}

OTHER_FILES += \
    codemirror/lib/codemirror.js \
    codemirror/lib/codemirror.css \
    codemirror/addon/search/searchcursor.js \
    codemirror/addon/search/search.js \
    codemirror/addon/dialog/dialog.js \
    codemirror/addon/dialog/dialog.css \
    codemirror/addon/edit/matchbrackets.js \
    codemirror/mode/prolog/prolog.js \
    codemirror/mode/prolog/index.html \
    CodeMirror.html \
    codemirror/LICENSE

RESOURCES += \
    lqUty.qrc

DISTFILES += \
    qwebchannel.js
