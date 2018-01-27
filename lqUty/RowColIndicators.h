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

#ifndef ROWCOLINDICATORS_H
#define ROWCOLINDICATORS_H

#include "lqUty_global.h"
#include <QLabel>
#include <QStatusBar>
#include <QPointer>

/** a class with the only purpose to display
 *  updated row/col number from QTextEditors cursor position
 */
class LQUTYSHARED_EXPORT RowColIndicators : QObject
{
    Q_OBJECT

public:

    /** create labels to hold changing numbers */
    void initializeStatusBar(QStatusBar *bar);

    QPointer<QLabel> rowIndicator, colIndicator, posIndicator;

    /** decoding QTextCursor position */
    void showCursorPosition(QObject* c);

public slots:

    void cursorPositionChanged();
};

#endif // ROWCOLINDICATORS_H
