#--------------------------------------------------
# lqUty.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Author       : Carlo Capelli
# Copyright @ 2023

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

# enable dependency tracking
CONFIG += create_prl

SOURCES += \
    ansi_esc_seq.cpp \
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
    ansi_esc_seq.h \
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

# removed jquery.min.js and qwebchannel.js
