#--------------------------------------------------
# spqr.pro: SWI-Prolog Qt Rendering
#--------------------------------------------------
# my first loqt component
#--------------------------------------------------
# Ing. Capelli Carlo - Brescia 2013

# please, not obsolete compiler
QMAKE_CXXFLAGS += -std=c++0x

QT += core gui webkit
greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = spqr
TEMPLATE = app

SOURCES += \
    main.cpp \
    mainwindow.cpp \
    SimPrologEdit.cpp \
    HelpDocView.cpp \
    cgraph_PL_cpp.cpp

HEADERS  += \
    mainwindow.h \
    SimPrologEdit.h \
    HelpDocView.h

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
    prolog/gv_uty.pl

RESOURCES += \
    spqr.qrc
