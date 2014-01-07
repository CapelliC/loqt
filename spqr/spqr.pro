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
    cgraph_PL_cpp.cpp \
    spqrMainWindow.cpp

HEADERS  += \
    SimPrologEdit.h \
    HelpDocView.h \
    spqrMainWindow.h

unix:!symbian:!macx {
    # because SWI-Prolog is built from source
    CONFIG += link_pkgconfig
    PKGCONFIG += swipl
}

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../lqXDot/release/ -llqXDot
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../lqXDot/debug/ -llqXDot
else:symbian: LIBS += -llqXDot
else:unix: LIBS += -L$$OUT_PWD/../lqXDot/ -llqXDot

INCLUDEPATH += $$PWD/../lqXDot
DEPENDPATH += $$PWD/../lqXDot

win32:CONFIG(release, debug|release): LIBS += -L$$PWD/../../SwiPlay-build-desktop-Desktop_Qt_4_8_1_for_GCC__Qt_SDK__Debug/pqConsole/release/ -lpqConsole
else:win32:CONFIG(debug, debug|release): LIBS += -L$$PWD/../../SwiPlay-build-desktop-Desktop_Qt_4_8_1_for_GCC__Qt_SDK__Debug/pqConsole/debug/ -lpqConsole
else:symbian: LIBS += -lpqConsole
else:unix: LIBS += -L$$PWD/../../SwiPlay-build-desktop-Desktop_Qt_4_8_1_for_GCC__Qt_SDK__Debug/pqConsole/ -lpqConsole

INCLUDEPATH += $$PWD/../../SwiPlay/pqConsole
DEPENDPATH += $$PWD/../../SwiPlay/pqConsole

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../lqUty/release/ -llqUty
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../lqUty/debug/ -llqUty
else:symbian: LIBS += -llqUty
else:unix: LIBS += -L$$OUT_PWD/../lqUty/ -llqUty

INCLUDEPATH += $$PWD/../lqUty
DEPENDPATH += $$PWD/../lqUty

OTHER_FILES += \
    prolog/gv_uty.pl \
    test/family.pl \
    test/familiari.pl \
    test/paths_prefix.pl \
    test/file_xref.pl

RESOURCES += \
    spqr.qrc
