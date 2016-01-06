#--------------------------------------------------
# pqXml.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Author        : Carlo Capelli
# Copyright (C) : 2015

include(../loqt.pri)
QT += svg xml xmlpatterns

TARGET = pqXml
TEMPLATE = lib

DEFINES += PQXML_LIBRARY

SOURCES += \
    pqXml.cpp \
    pqXmlView.cpp

HEADERS += \
    pqXml.h \
    pqXml_global.h \
    pqXmlView.h

unix {
    target.path = /usr/lib
    INSTALLS += target

    CONFIG += link_pkgconfig
    PKGCONFIG += swipl
}

# include utilities

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../lqUty/release/ -llqUty
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../lqUty/debug/ -llqUty
else:unix: LIBS += -L$$OUT_PWD/../lqUty/ -llqUty

INCLUDEPATH += $$PWD/../lqUty
DEPENDPATH += $$PWD/../lqUty

# include Prolog console

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../pqConsole/release/ -lpqConsole
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../pqConsole/debug/ -lpqConsole
else:unix: LIBS += -L$$OUT_PWD/../pqConsole/ -lpqConsole

INCLUDEPATH += $$PWD/../pqConsole
DEPENDPATH += $$PWD/../pqConsole
