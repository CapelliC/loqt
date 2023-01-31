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

#ifndef KEYBOARDMACROS_H
#define KEYBOARDMACROS_H

#include "lqUty_global.h"

#include <QMenu>
#include <QPointer>
#include <QKeyEvent>
#include <QShortcut>
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
    void setupMenu(QMenu *menu, bool sepBefore = false);

    /// resonable defaults for GUI bindings hotkeys
    static QKeySequence start();
    static QKeySequence stop();
    static QKeySequence play();
    static QShortcut *start_(QWidget *parent) { return new QShortcut(start(), parent); }
    static QShortcut *stop_(QWidget *parent) { return new QShortcut(stop(), parent); }
    static QShortcut *play_(QWidget *parent) { return new QShortcut(play(), parent); }

    void manage(QWidget *keybHost);

    /// TBD provide named macros with fast startup
    void storeEvent(QKeyEvent *e);
    static QString defaultName() { return "<macro>"; }
    QString currName() const { return defaultName(); }
    void setLastRecordedName(QString name);

protected:
    bool eventFilter(QObject *obj, QEvent *event);

signals:

    void registerCompleted();
    void playbackCompleted();

    //! basic user feedback about interface status
    void feedback(QString msg);

public slots:

    void startRecording(QWidget*);
    void stopRecording(QWidget*);
    void Playback(QWidget*);

private:

    //! restrict to keyboard events by now
    struct keystroke {
        QEvent::Type type;
        int key;
        Qt::KeyboardModifiers modifiers;
        QString text;
        bool autorep;
        quint16 count;
    };

    static keystroke e2k(const QKeyEvent &e);
    static QKeyEvent k2e(const keystroke& k);

    static keystroke s2k(const QString &s);
    static QString k2s(const keystroke& k);

    /*// serializing to store in preferences
    static QStringList e2l(const QKeyEvent &e);
    static QKeyEvent l2e(const QStringList &l);
    static QString e2s(const QKeyEvent &e);
//{ return e2l(e).join(","); }
    static QKeyEvent s2e(const QString &s);
//{ return l2e(s.split(",")); }
*/


    typedef QList<keystroke> macro;
    QMap<QString, macro> macros;


    //! TBD allows to reference macros by name
    QString lastRecorded;

    enum { idle, onRecord, onPlayback } status;

    QPointer<QAction>
        macroStartRegAct,
        macroStopRegAct,
        macroPlaybackAct,
        macroSelectAct;
};

#endif // KEYBOARDMACROS_H
