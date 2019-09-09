package net.noresttherein.slang.logging;

import java.util.function.Function;
import java.util.logging.LogRecord;
import java.util.stream.Stream;
import java.lang.StackWalker.StackFrame;

class FindCaller {

    public static StackFrame apply() {
        return StackWalker.getInstance().walk(frames -> {
            var it = frames.iterator();
            StackFrame frame = null;
            while (it.hasNext()) {
                frame = it.next();
                String className = frame.getClassName();
                if (!className.startsWith("net.noresttherein.slang.logging")) {
                    return frame;
                }
            }
            return frame;
        });
    }

    public static void fill(final LogRecord record) {
        StackWalker.getInstance().walk(frames -> {
            var it = frames.iterator();
            StackFrame frame = null;
            while (it.hasNext()) {
                frame = it.next();
                String className = frame.getClassName();
                if (!className.startsWith("net.noresttherein.slang.logging")) {
                    record.setSourceClassName(className + "(" + frame.getFileName() + ":" + frame.getLineNumber() + ")");
                    record.setSourceMethodName(frame.getMethodName());
                    record.setParameters(new Object[]{ frame.getFileName(), frame.getLineNumber() });
                    return frame;
                }
            }
            if (frame != null) {
                record.setSourceClassName(frame.getClassName() + "(" + frame.getFileName() + ":" + frame.getLineNumber() + ")");
                record.setSourceMethodName(frame.getMethodName());
                record.setParameters(new Object[]{ frame.getFileName(), frame.getLineNumber() });
            }
            return frame;
        });
    }

}
