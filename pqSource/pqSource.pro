#    pqSource     : interfacing SWI-Prolog source files and Qt
#
#    E-mail       : cc.carlo.cap@gmail.com
#    Author       : Carlo Capelli
#    Copyright (C): 2013,2014,2015,2016,2017,2018,2019


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

include(../loqt.pri)
QT += concurrent

TARGET = pqSource
TEMPLATE = lib

DEFINES += PQSOURCE_LIBRARY

# enable dependency tracking
CONFIG += create_prl

unix {

    # because SWI-Prolog is built from source
    CONFIG += link_pkgconfig
    PKGCONFIG += swipl

    # Graphviz display of structured graphs
    DEFINES += WITH_CGRAPH
    PKGCONFIG += libcgraph libgvc
}

INCLUDEPATH += $$PWD/../lqUty
DEPENDPATH += $$PWD/../lqUty

INCLUDEPATH += $$PWD/../pqConsole
DEPENDPATH += $$PWD/../pqConsole

INCLUDEPATH += $$PWD/../pqGraphviz
DEPENDPATH += $$PWD/../pqGraphviz

INCLUDEPATH += $$PWD/../lqXDot
DEPENDPATH += $$PWD/../lqXDot

INCLUDEPATH += $$PWD/../pqXml
DEPENDPATH += $$PWD/../pqXml


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
    MdiChildWithCheck.cpp \
    pqWebScript.cpp \
    proofGraph.cpp

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
    MdiChildWithCheck.h \
    pqWebScript.h \
    proofGraph.h

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
    prolog/calledgraph.pl \
    prolog/pqSourceFileXref.pl
