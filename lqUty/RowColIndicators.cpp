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

#include "RowColIndicators.h"
#include <QTextEdit>
#include <QTextBlock>

void RowColIndicators::initializeStatusBar(QStatusBar *bar) {
    bar->insertPermanentWidget(0, rowIndicator = new QLabel);
    rowIndicator->setFrameStyle(QFrame::Panel | QFrame::Sunken);
    rowIndicator->setText("#Row");
    rowIndicator->setToolTip(tr("Show row number of current document"));

    bar->insertPermanentWidget(1, colIndicator = new QLabel);
    colIndicator->setFrameStyle(QFrame::Panel | QFrame::Sunken);
    colIndicator->setText("#Col");
    colIndicator->setToolTip(tr("Show column number of current document"));

    bar->insertPermanentWidget(2, posIndicator = new QLabel);
    posIndicator->setFrameStyle(QFrame::Panel | QFrame::Sunken);
    posIndicator->setText("#Pos");
    posIndicator->setToolTip(tr("Show absolute position in current document"));
}

void RowColIndicators::showCursorPosition(QObject* s) {
    if (auto e = qobject_cast<QTextEdit*>(s)) {
        auto c = e->textCursor();
        rowIndicator->setText(QString::number(c.block().blockNumber() + 1));
        colIndicator->setText(QString::number(c.positionInBlock() + 1));
        posIndicator->setText(QString::number(c.position()));
    }
}

void RowColIndicators::cursorPositionChanged() {
    showCursorPosition(sender());
}
