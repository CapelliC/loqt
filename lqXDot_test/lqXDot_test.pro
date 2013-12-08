#--------------------------------------------------
# lqXDot_test.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Ing. Capelli Carlo - Brescia 2013

QT += core gui svg

TARGET = lqXDot_test
TEMPLATE = app

QMAKE_CXXFLAGS += -std=c++0x

unix {
    CONFIG += link_pkgconfig

    DEFINES += WITH_CGRAPH
    # latest download (2013/11/29) refuses to compile without this useless define
    DEFINES += HAVE_STRING_H

    PKGCONFIG += libcgraph libgvc
}

DEFINES += QT_NO_OPENGL

SOURCES += \
    main.cpp \
    mainwindow.cpp \
    SvgView.cpp \
    XmlSyntaxHighlighter.cpp

HEADERS += \
    mainwindow.h \
    SvgView.h \
    XmlSyntaxHighlighter.h

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../lqXDot/release/ -llqXDot
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../lqXDot/debug/ -llqXDot
else:unix:!symbian: LIBS += -L$$OUT_PWD/../lqXDot/ -llqXDot

INCLUDEPATH += $$PWD/../lqXDot
DEPENDPATH += $$PWD/../lqXDot

OTHER_FILES += \
    gallery/images/rosemary.jpg \
    gallery/images/Rose_kennedy.JPG \
    gallery/images/n48862003257_1275276_1366.jpg \
    gallery/images/kennedyface.jpg \
    gallery/images/jacqueline-kennedy-onassis.jpg \
    gallery/images/BE037819.jpg \
    gallery/images/1025901671.jpg \
    gallery/images/180px-JFKJr2.jpg \
    gallery/images/165px-Caroline_Kennedy.jpg \
    gallery/crazy.gv \
    gallery/cluster.gv \
    gallery/angles.gv

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../lqUty/release/ -llqUty
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../lqUty/debug/ -llqUty
else:symbian: LIBS += -llqUty
else:unix: LIBS += -L$$OUT_PWD/../lqUty/ -llqUty

INCLUDEPATH += $$PWD/../lqUty
DEPENDPATH += $$PWD/../lqUty
