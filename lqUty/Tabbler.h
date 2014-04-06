/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014 Carlo Capelli

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

#ifndef TABBLER_H
#define TABBLER_H

#include <QPointer>
#include <QTextEdit>
#include <QTextFrame>
#include <QTextTable>

/** utility to add tables in a QTextEdit view
 */
namespace Tabbler {

    typedef QStringList Row;
    typedef QList<Row> Tbl;

    struct TblBuild {

        TblBuild(QTextEdit *te) : count(0) {
            cursor = te->textCursor();
            topFrame = cursor.currentFrame();
        }

        void operator +=(const Tbl &y) { addTable(y); }

        void addTable(const Tbl &t) {
            cursor.setPosition(topFrame->lastPosition());
            if (count++)
                cursor.insertBlock();

            QTextTable *tab = cursor.insertTable(t.count(), t[0].count());
            for (int r = 0; r < t.count(); ++r)
                for (int c = 0; c < t[r].count(); ++c)
                    tab->cellAt(r, c).firstCursorPosition().insertText(t[r][c]);
        }

        QTextCursor cursor;
        QPointer<QTextFrame> topFrame;
        int count;
    };
}

#endif // TABBLER_H
