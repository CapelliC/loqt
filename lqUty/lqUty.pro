#--------------------------------------------------
# lqUty.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Ing. Capelli Carlo - Brescia 2013

QT += webkit
TARGET = lqUty
TEMPLATE = lib
QMAKE_CXXFLAGS += -std=c++0x

DEFINES += LQUTY_LIBRARY

SOURCES += lqUty.cpp \
    ParenMatching.cpp \
    MruHelper.cpp \
    RowColIndicators.cpp \
    lqPreferences.cpp \
    QStackedWidget_KeybTabs.cpp \
    SourceEdit.cpp

HEADERS += lqUty.h \
    lqUty_global.h \
    ParenMatching.h \
    do_events.h \
    MruHelper.h \
    RowColIndicators.h \
    lqPreferences.h \
    blockSig.h \
    QStackedWidget_KeybTabs.h \
    SourceEdit.h \
    file2string.h

symbian {
    MMP_RULES += EXPORTUNFROZEN
    TARGET.UID3 = 0xE3BE79CA
    TARGET.CAPABILITY = 
    TARGET.EPOCALLOWDLLDATA = 1
    addFiles.sources = lqUty.dll
    addFiles.path = !:/sys/bin
    DEPLOYMENT += addFiles
}

unix:!symbian {
    maemo5 {
        target.path = /opt/usr/lib
    } else {
        target.path = /usr/lib
    }
    INSTALLS += target
}

OTHER_FILES += \
    SourceEdit.html \
    codemirror/lib/codemirror.js \
    codemirror/lib/codemirror.css \
    codemirror/addon/search/searchcursor.js \
    codemirror/addon/search/search.js \
    codemirror/addon/dialog/dialog.js \
    codemirror/addon/dialog/dialog.css \
    codemirror/addon/edit/matchbrackets.js \
    codemirror/mode/prolog/prolog.js \
    codemirror/mode/prolog/index.html

RESOURCES += \
    lqUty.qrc