/*
    pqSource     : interfacing SWI-Prolog source files and Qt

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

#ifndef PQHIGHLIGHTER_H
#define PQHIGHLIGHTER_H

#include <QSyntaxHighlighter>
#include <QTextBlock>
#include <QPointer>
#include <QMutex>

#include "pqSyntaxData.h"
#include "pqMiniSyntax.h"

/** highlighter specialized to handle nested categories
 *  categorization performed by SWI-Prolog library(syntax_colour)
 */
class pqHighlighter : public pqMiniSyntax {
    Q_OBJECT
public:

    pqHighlighter(QTextEdit *host);
    pqHighlighter(QTextEdit *host, pqSyntaxData *pData);

    //! reapply to lines
    void rehighlightLines(ParenMatching::range position);

    //! apply rules to requested text block (say, a line...)
    void highlightBlock(const QTextBlock &b, bool withFeedback = true);

    //! holds actual syntax info from Prolog
    friend class pqSyntaxData;
    QPointer<pqSyntaxData> pData;

protected:

    //! apply configuration. Relies on currentBlock() to access actual text position
    virtual void highlightBlock(const QString &text);

    //! serialize access to UserBlockData formatting
    QMutex blockSer;

public slots:

signals:

    void highlightComplete();
};

#endif // PQHIGHLIGHTER_H
