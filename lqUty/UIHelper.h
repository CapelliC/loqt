/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright @ 2023

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

#ifndef UIHELPER_H
#define UIHELPER_H

#include "lqUty_global.h"

#include <QMenu>
#include <QWidget>
#include <QPointer>
#include <QToolBar>
#include <QActionGroup>

class LQUTYSHARED_EXPORT UIHelper
{
public:
    UIHelper(QWidget *host = nullptr);

    struct Actions {
        QPointer<QMenu> menu;
        QPointer<QToolBar> toolBar;
        QPointer<QActionGroup> actionGroup;
    };

    Actions macro;
    Actions &setupMacro(QWidget* stage);

};

#endif // UIHELPER_H
