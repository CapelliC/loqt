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

#ifndef KEYBOARDMACROS_H
#define KEYBOARDMACROS_H

#include "EditInterface.h"
#include "lqPreferences.h"

#include <QKeyEvent>
#include <QShortcut>
#include <QMenu>
#include <QStatusBar>

/** helper class to record and playback macros
 */
class LQUTYSHARED_EXPORT KeyboardMacros : public QObject
{
    Q_OBJECT
public:

    explicit KeyboardMacros(QObject *parent = 0);
    ~KeyboardMacros();

    /// connect to typical QMainWindow environment
    void setupGUI(QStatusBar *bar, QMenu *menu);

    /// resonable defaults for GUI bindings hotkeys
    static QKeySequence start() { return QKeySequence("Ctrl+Shift+R"); }
    static QKeySequence stop() { return QKeySequence("Ctrl+Shift+T"); }
    static QKeySequence play() { return QKeySequence("Ctrl+Shift+Y"); }

    static QShortcut *start_(QWidget *parent) { return new QShortcut(start(), parent); }
    static QShortcut *stop_(QWidget *parent) { return new QShortcut(stop(), parent); }
    static QShortcut *play_(QWidget *parent) { return new QShortcut(play(), parent); }

    typedef const EditInterface& editor;

    void startPlayback(QString name, editor);

    /// provide named macros with fast startup
    void storeEvent(QKeyEvent *e);
    static QString defaultName() { return "<macro>"; }
    void setLastRecordedName(QString name);

    /// serializing to store in preferences
    static QStringList e2l(const QKeyEvent &e);
    static QKeyEvent l2e(const QStringList &l);
    static QString e2s(const QKeyEvent &e) { return e2l(e).join(","); }
    static QKeyEvent s2e(const QString &s) { return l2e(s.split(",")); }

protected:
    bool eventFilter(QObject *obj, QEvent *event);

signals:
    void playbackCompleted();
    void registerCompleted();

public slots:

    void startRecording(QTextEdit *);
    void doneRecording();
    void startPlayback(QTextEdit *);

private:

    typedef QList<QKeyEvent> macro;
    QMap<QString, macro> macros;

    QString lastRecorded;

    EditInterface binding;
};

#endif // KEYBOARDMACROS_H
