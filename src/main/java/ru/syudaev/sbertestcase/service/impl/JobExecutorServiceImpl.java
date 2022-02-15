package ru.syudaev.sbertestcase.service.impl;

import java.util.List;
import java.util.concurrent.locks.ReentrantLock;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;

import ru.syudaev.sbertestcase.jobs.ThreadJob;
import ru.syudaev.sbertestcase.service.AccumulatorService;
import ru.syudaev.sbertestcase.service.JobExecutorService;

/**
 * Сервис запуска джобов в параллельных потоках.
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class JobExecutorServiceImpl implements JobExecutorService {

    private final List<ThreadJob> threadJobs;
    private final AccumulatorService accumulatorService;

    /**
     * Запускается при обновлении контекста.
     *
     * @param event ContextRefreshedEvent event.
     */
    @EventListener
    private void onContextRefresh(ContextRefreshedEvent event) {
        fillTable();
        executeMultiThreadJob();
    }

    /**
     * Инициализирует исходные данные.
     */
    @Override
    public void fillTable() {
        accumulatorService.accumulateInitialData();
    }

    /**
     * Запусакет джобы.
     */
    public void executeMultiThreadJob() {
        ReentrantLock lock = new ReentrantLock(true);
        threadJobs.forEach(job -> new Thread(() -> job.execute(lock)).start());
    }
}
