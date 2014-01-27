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

#include "FindReplace.h"
#include "blockSig.h"

#include <QComboBox>
#include <QPushButton>
#include <QFormLayout>
#include <QGridLayout>
#include <QCheckBox>
#include <QTextCursor>
#include <QTimer>
#include <QSyntaxHighlighter>

/** construct UI objects with default UX
 *  retrieve previous settings from lqPreferences
 */
FindReplace::FindReplace(QWidget *parent) :
    QDialog(parent),

    find(tr("&Find")),
    findNext(tr("Find &Next")),
    replace(tr("&Replace")),
    replaceFind(tr("Replace && Find")),
    replaceAll(tr("Replace &All")),

    regex(tr("Rege&x")),
    wholeWord(tr("&Whole Word")),
    caseSensitive(tr("Case &Sensitive")),
    backward(tr("Search &Backward"))
{
    setWindowTitle(tr("Find/Replace Text"));
    //setAttribute(Qt::WA_DeleteOnClose);

    find.setDefault(true);

    to_search.setEditable(true);
    to_replace.setEditable(true);

    QFormLayout *l = new QFormLayout;
    l->addRow(tr("&Pattern"), &to_search);
    l->addRow(tr("&Replace"), &to_replace);

    QHBoxLayout *h = new QHBoxLayout;
    h->addWidget(&find);
    h->addWidget(&findNext);
    h->addWidget(&replace);
    h->addWidget(&replaceFind);
    h->addWidget(&replaceAll);

    QHBoxLayout *b = new QHBoxLayout;
    b->addWidget(&regex);
    b->addWidget(&wholeWord);
    b->addWidget(&caseSensitive);
    b->addWidget(&backward);

    QVBoxLayout *v = new QVBoxLayout;
    v->addLayout(l);
    v->addLayout(h);
    v->addLayout(b);
    setLayout(v);

    to_search.setToolTip(tr("Specify the text to search."));
    to_replace.setToolTip(tr("Specify the text to replace when searched text is found."));
    find.setToolTip(tr("Find the first occurrence of text from cursor."));
    findNext.setToolTip(tr("Find the next occurrence of text, starting from last found."));
    replace.setToolTip(tr("Replace current found text instance."));
    replaceFind.setToolTip(tr("Replace current found text instance and search next."));
    replaceAll.setToolTip(tr("Replace all instances of matching text."));
    regex.setToolTip(tr("Use a <a href='http://qt-project.org/doc/qt-5.0/qtcore/qregexp.html'>Regular Expression</a>\nto search the specified pattern."));
    backward.setToolTip(tr("Search backwards instead of forwards."));
    caseSensitive.setToolTip(tr("By default find works case insensitive.\nSpecifying this option changes the behaviour to a case sensitive find operation."));
    wholeWord.setToolTip(tr("Makes find match only complete words."));

    connect(&find, SIGNAL(clicked()), this, SLOT(onFind()));
    connect(&findNext, SIGNAL(clicked()), this, SLOT(onFindNext()));

    lqPreferences p;
    p.beginGroup("FindReplace");
    to_search.addItems(p.value("search").toStringList());
    to_replace.addItems(p.value("replace").toStringList());
    regex.setChecked(p.value("regex").toBool());
    backward.setChecked(p.value("backward").toBool());
    caseSensitive.setChecked(p.value("caseSensitive").toBool());
    wholeWord.setChecked(p.value("wholeWord").toBool());
    p.endGroup();
}

/** commit user current settings to lqPreferences
 */
FindReplace::~FindReplace()
{
    lqPreferences p;
    p.beginGroup("FindReplace");

    auto items = [](QComboBox &c) {
        QStringList l;
        for (int i = 0; i < c.count(); ++i)
            l << c.itemText(i);
        return l;
    };
    p.setValue("search", items(to_search));
    p.setValue("replace", items(to_replace));

    p.setValue("regex", regex.isChecked());
    p.setValue("backward", backward.isChecked());
    p.setValue("caseSensitive", caseSensitive.isChecked());
    p.setValue("wholeWord", wholeWord.isChecked());

    p.endGroup();
}

/** apply current settings to buffer
 */
void FindReplace::do_find(QTextEdit *target)
{
    this->target = target;

    QTextCursor c = target->textCursor();
    if (c.hasSelection())
        to_search.setEditText(c.selectedText());
    //emit onFind();
    show();
}

void FindReplace::do_findNext(QTextEdit *target)
{
    this->target = target;
    emit onFindNext();
}

void FindReplace::do_findPrevious(QTextEdit *target)
{
    this->target = target;
    backward.setChecked(!backward.isChecked());
    emit onFindNext();
}

void FindReplace::do_replace(QTextEdit *target)
{
    this->target = target;
    show();
    emit onReplace();
}

/** decode UI current settings
 */
QTextDocument::FindFlags FindReplace::flags() const
{
    QTextDocument::FindFlags f = 0;
    if (backward.isChecked())
        f |= QTextDocument::FindBackward;
    if (caseSensitive.isChecked())
        f |= QTextDocument::FindCaseSensitively;
    if (wholeWord.isChecked())
        f |= QTextDocument::FindWholeWords;
    return f;
}

/** apply to current document
 */
QTextCursor FindReplace::start()
{
    QString s = to_search.currentText();
    QTextCursor c = target->textCursor();
    if (regex.isChecked())
        return target->document()->find(QRegExp(s), c, flags());
    else
        return target->document()->find(s, c, flags());
}

/** put visual clue about found items
 */
void FindReplace::mark(QTextCursor c, bool current, bool repl)
{
    emit markCursor(c);
    emit outcome(tr("Text has been found."));

    if (current) {
        target->setTextCursor(c);
        target->ensureCursorVisible();
    }

    if (repl) {
        c.removeSelectedText();
        c.insertText(to_replace.currentText());

        emit outcome(tr("Text has been replaced."));
    }
}

/** serve user request
 */
void FindReplace::onFind()
{
    QTextCursor c = start();
    if (c.hasSelection()) {
        mark(c, true);
        hide();
    }
    else notfound();
}

/** serve user request: find next occurrence
 */
void FindReplace::onFindNext()
{
    QTextCursor c = start();
    if (c.hasSelection()) {
        mark(c, true);
    }
    else notfound();
}

/** serve user request: replace current occurrence
 */
void FindReplace::onReplace()
{
    if (canReplace()) {
        QTextCursor c = start();
        if (c.hasSelection())
            mark(c, true, true);
        else notfound();
    }
}

/** replace current occurrence, find next
 */
void FindReplace::onReplaceFind()
{
    if (canReplace()) {
        QTextCursor c = start();
        if (c.hasSelection()) {
            mark(c, true, true);
            QTimer::singleShot(10, this, SLOT(onFindNext()));
        }
        else notfound();
    }
}

/** replace all occurrences from cursor point
 */
void FindReplace::onReplaceAll()
{
    if (canReplace()) {
        QTextCursor c = start();
        if (c.hasSelection()) {
            mark(c, true, true);
            QTimer::singleShot(10, this, SLOT(onReplaceAll()));
        }
        else notfound();
    }
}

/** emit a message to warn user that selection not has been found
 */
void FindReplace::notfound()
{
    emit outcome(tr("Text '%1' not found.").arg(to_search.currentText()));
}
