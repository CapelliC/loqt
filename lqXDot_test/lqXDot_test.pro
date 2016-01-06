#--------------------------------------------------
# lqXDot_test.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Author        : Carlo Capelli
# Copyright (C): 2013,2014,2015,2016

QT += gui svg widgets

TARGET = lqXDot_test
TEMPLATE = app
CONFIG += C++11

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
    SvgView.cpp

HEADERS += \
    mainwindow.h \
    SvgView.h

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
    gallery/angles.gv \
    gallery/wos.gv \
    gallery/world.gv \
    gallery/unix.gv \
    gallery/twopi2.gv \
    gallery/tree test.gv \
    gallery/tree_foldable.gv \
    gallery/transparency.gv \
    gallery/traffic_lights.gv \
    gallery/test_colors.gv \
    gallery/table.xdot \
    gallery/table.gv \
    gallery/switch.gv \
    gallery/softmaint.gv \
    gallery/siblings.gv \
    gallery/sdh.gv \
    gallery/root.gv \
    gallery/radial_angle.gv \
    gallery/psg.gv \
    gallery/profile.gv \
    gallery/philo.gv \
    gallery/networkmap_twopi.gv \
    gallery/lion_share.gv \
    gallery/linear_angle.gv \
    gallery/kennedyanc.gv \
    gallery/html3.gv \
    gallery/html2.gv \
    gallery/html1.gv \
    gallery/Genetic_Programming.gv \
    gallery/gd_1994_2007.gv \
    gallery/g_c_n.gv \
    gallery/fsm.gv \
    gallery/fdpclust.gv \
    gallery/ER.gv \
    gallery/debugger.gv \
    gallery/datastruct_gradient.gv \
    gallery/datastruct.gv \
    gallery/colors.gv \
    gallery/cluster_gradient.gv \
    gallery/tree test.gv

win32:CONFIG(release, debug|release): LIBS += -L$$OUT_PWD/../lqUty/release/ -llqUty
else:win32:CONFIG(debug, debug|release): LIBS += -L$$OUT_PWD/../lqUty/debug/ -llqUty
else:symbian: LIBS += -llqUty
else:unix: LIBS += -L$$OUT_PWD/../lqUty/ -llqUty

INCLUDEPATH += $$PWD/../lqUty
DEPENDPATH += $$PWD/../lqUty
