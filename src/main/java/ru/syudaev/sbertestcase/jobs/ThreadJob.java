package ru.syudaev.sbertestcase.jobs;

import java.util.concurrent.locks.ReentrantLock;

/**
 * Джоба для многопоточной обработки.
 */
public interface ThreadJob {

    void execute(ReentrantLock lock);
}
