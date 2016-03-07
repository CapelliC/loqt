#--------------------------------------------------
# lqGraphix    : SWI-Prolog and Qt Graphics Framework
#--------------------------------------------------
# Author       : Carlo Capelli
# Copyright (C): 2016

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
    mainwindow.cpp \
    lqGraphixView.cpp \
    lqGraphixScene.cpp

HEADERS += \
    mainwindow.h \
    lqGraphixView.h \
    lqGraphixScene.h \
    primitives.h

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
