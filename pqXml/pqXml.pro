#-------------------------------------------------
#
# Project created by QtCreator 2015-12-23T15:43:40
#
#-------------------------------------------------

QT += widgets svg xml xmlpatterns
CONFIG += C++11

TARGET = pqXml
TEMPLATE = lib

DEFINES += PQXML_LIBRARY

SOURCES += pqXml.cpp \
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

# include Prolog console
win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../pqConsole/release/ -lpqConsole
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../pqConsole/debug/ -lpqConsole
else:unix: LIBS += -L$$OUT_PWD/../pqConsole/ -lpqConsole

INCLUDEPATH += $$PWD/../pqConsole
DEPENDPATH += $$PWD/../pqConsole

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../lqUty/release/ -llqUty
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../lqUty/debug/ -llqUty
else:unix:!symbian: LIBS += -L$$OUT_PWD/../lqUty/ -llqUty

INCLUDEPATH += $$PWD/../lqUty
DEPENDPATH += $$PWD/../lqUty
