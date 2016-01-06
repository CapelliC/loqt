/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016

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

#include "QStackedWidget_KeybTabs.h"
#include <QKeyEvent>
#include <QDebug>

/** handle keyboard to switch among stacked widgets
 */
QStackedWidget_KeybTabs::QStackedWidget_KeybTabs(QWidget *parent) :
    QStackedWidget(parent)
{
}

/** handle TAB and BACKTAB with modifiers
 */
void QStackedWidget_KeybTabs::keyPressEvent(QKeyEvent *e)
{
    if (e->modifiers() & Qt::CTRL) {
        if (e->modifiers() & Qt::SHIFT) {
            if (e->key() == Qt::Key_Backtab)
                setCurrentIndex(currentIndex() > 0 ? currentIndex() - 1 : count() - 1);
        }
        else if (e->key() == Qt::Key_Tab)
            setCurrentIndex((currentIndex() + 1) % count());
    }
}
