#--------------------------------------------------
# loqt.pro: Logic / Qt interface
#--------------------------------------------------
# Collection of Qt components to efficiently
# interface logic languages
#--------------------------------------------------
# Author        : Carlo Capelli
# Copyright (C) : 2013,2014,2015,2016

# please, an up-to-date compiler
CONFIG += C++11

QT += core gui widgets

greaterThan(QT_VERSION, "5.5.0"): {
    QT += webenginewidgets
    DEFINES += QT_WEBENGINE_LIB
} else {
    QT += webkitwidgets
}
