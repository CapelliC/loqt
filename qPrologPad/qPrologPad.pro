#--------------------------------------------------
# qPrologPad.pro: SWI-Prolog PrologPad in Qt
#--------------------------------------------------
# prologpad courtesy Jan Wielemaker
#--------------------------------------------------
# Copyright (C) : 2014,2015 Carlo Capelli

QMAKE_CXXFLAGS += -std=c++0x

QT += core gui widgets webkit webkitwidgets

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
else:unix:!symbian: LIBS += -L$$OUT_PWD/../pqConsole/ -lpqConsole

INCLUDEPATH += $$PWD/../pqConsole
DEPENDPATH += $$PWD/../pqConsole

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../pqGraphviz/release/ -lpqGraphviz
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../pqGraphviz/debug/ -lpqGraphviz
else:unix:!symbian: LIBS += -L$$OUT_PWD/../pqGraphviz/ -lpqGraphviz

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
