/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016,2017,2018

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "lqPreferences.h"
#include <QDebug>
#include <QWidget>

/** get configured values, with reasonable defaults
 */
lqPreferences::lqPreferences(QObject *parent) :
    QSettings("C & C", "lqUty", parent)
{
}

void lqPreferences::save() {

}

void lqPreferences::loadGeometry(QString key, QWidget *w) {
    w->restoreGeometry(value(key + "/geometry").toByteArray());
}
void lqPreferences::saveGeometry(QString key, QWidget *w) {
    setValue(key + "/geometry", w->saveGeometry());
}
void lqPreferences::loadGeometry(QWidget *w) {
    loadGeometry(w->metaObject()->className(), w);
}
void lqPreferences::saveGeometry(QWidget *w) {
    saveGeometry(w->metaObject()->className(), w);
}

void lqPreferences::loadPosSizeState(QString key, QWidget *w) {
    beginGroup(key);
    QPoint pos = value("pos", QPoint(40, 30)).toPoint();
    QSize size = value("size", QSize(400, 300)).toSize();
    int state = value("state", static_cast<int>(Qt::WindowNoState)).toInt();
    w->move(pos);
    w->resize(size);
    w->setWindowState(static_cast<Qt::WindowStates>(state));
    endGroup();
}

void lqPreferences::savePosSizeState(QString key, QWidget *w) {
    beginGroup(key);
    setValue("pos", w->pos());
    setValue("size", w->size());
    setValue("state", static_cast<int>(w->windowState()));
    endGroup();
}
