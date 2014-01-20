#--------------------------------------------------
# spqr.pro: SWI-Prolog Qt Rendering
#--------------------------------------------------
# my first loqt component
#--------------------------------------------------
# Copyright (C) : 2013,2014 Carlo Capelli

# please, not obsolete compiler
QMAKE_CXXFLAGS += -std=c++0x

QT += core gui webkit
greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = spqr
TEMPLATE = app

SOURCES += \
    main.cpp \
    SimPrologEdit.cpp \
    HelpDocView.cpp \
    spqrMainWindow.cpp

HEADERS  += \
    SimPrologEdit.h \
    HelpDocView.h \
    spqrMainWindow.h

OTHER_FILES += \
    prolog/newFileDefaultScript.pl

RESOURCES += \
    spqr.qrc

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
