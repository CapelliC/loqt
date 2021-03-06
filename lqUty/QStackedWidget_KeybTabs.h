/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016,2017,2018,2019

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

#ifndef QSTACKEDWIDGET_KEYBTABS_H
#define QSTACKEDWIDGET_KEYBTABS_H

#include "lqUty_global.h"
#include <QStackedWidget>

/** handle keyboard to switch among stacked widgets
 */
class LQUTYSHARED_EXPORT QStackedWidget_KeybTabs : public QStackedWidget
{
    Q_OBJECT
public:

    explicit QStackedWidget_KeybTabs(QWidget *parent = 0);
    
signals:
    
public slots:

protected:

    //! handle TAB and BACKTAB
    virtual void keyPressEvent(QKeyEvent *);
};

#endif // QSTACKEDWIDGET_KEYBTABS_H
