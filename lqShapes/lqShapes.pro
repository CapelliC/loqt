#--------------------------------------------------
# lqShapes     : SWI-Prolog and Qt Graphics Framework
#--------------------------------------------------
# Author       : Carlo Capelli
# Copyright (C): 2016
#-------------------------------------------------

QT += widgets
CONFIG += C++11

TARGET = lqShapes
TEMPLATE = lib

DEFINES += LQSHAPES_LIBRARY

SOURCES += lqShapes.cpp \
    lqShapesView.cpp \
    lqShapesScene.cpp

HEADERS += lqShapes.h \
    lqShapes_global.h \
    lqShapesView.h \
    lqShapesScene.h

unix {
    # if SWI-Prolog is built from source
    CONFIG += link_pkgconfig
    PKGCONFIG += swipl
}

unix {
    target.path = /usr/lib
    INSTALLS += target
}

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
