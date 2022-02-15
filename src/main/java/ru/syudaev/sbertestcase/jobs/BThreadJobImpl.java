package ru.syudaev.sbertestcase.jobs;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import ru.syudaev.sbertestcase.entity.Accumulator;
import ru.syudaev.sbertestcase.entity.Result;
import ru.syudaev.sbertestcase.repository.AccumulatorRepository;
import ru.syudaev.sbertestcase.repository.ResultRepository;

/**
 * Джоба для многопоточной обработки.
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class BThreadJobImpl implements ThreadJob {

    private static final String THREAD_NAME = "ThreadB";

    private final AccumulatorRepository accumulatorRepository;
    private final ResultRepository resultRepository;

    /**
     * Содержит основную логику обработки сущности потоком.
     *
     * @param lock Лок.
     */
    @Override
    public void execute(ReentrantLock lock) {
        while (true) {
            try {
                if (lock.tryLock(10, TimeUnit.SECONDS)) {
                    log.debug("Поток {} взял лок", THREAD_NAME);
                    var accumulatorOpt = accumulatorRepository.getRandomAccumulatorOpt();

                    if (accumulatorOpt.isPresent()) {
                        var accumulator = accumulatorOpt.get();
                        log.debug("Поток {} захватил объект: {}", THREAD_NAME, accumulator);

                        if (Boolean.FALSE.equals(accumulator.getValue().getThreadB())) {
                            accumulator.getValue().setThreadB(true);

                            updateQueue(accumulator);

                            var savedEntity = accumulatorRepository.save(accumulator);
                            log.info("Поток {} закончил итерацию. Сохраненное значение: {}", THREAD_NAME, savedEntity);
                        } else if (Boolean.TRUE.equals(accumulator.getValue().getThreadA())) {
                            var savedResult = resultRepository.save(Result.from(accumulator));
                            log.info("Поток {} сохранил результат обработки: {}", THREAD_NAME, savedResult);

                            accumulatorRepository.delete(accumulator);
                            log.info("Поток {} удалил исходные данные {}", THREAD_NAME, accumulator);
                        }
                    } else {
                        log.info("Поток {} завершил работу: не найдены исходные данные для обработки.", THREAD_NAME);
                        return;
                    }
                } else {
                    throw new RuntimeException("Не удалось взять блокировку");
                }
            } catch (InterruptedException e) {
                log.warn("Не удалось выполнить операцию", e);
                throw new RuntimeException(e);
            } finally {
                if (lock.isHeldByCurrentThread()) {
                    lock.unlock();
                    log.debug("Поток {} сдал лок", THREAD_NAME);
                }
            }
        }
    }

    /**
     * Делает пометку о том, что текущий поток внес изменения в строку.
     *
     * @param accumulator {@link Accumulator}.
     */
    private void updateQueue(Accumulator accumulator) {
        var queue = accumulator.getValue().getQueue();

        for (int i = 0; i < queue.length; i++) {
            if (StringUtils.isBlank(queue[i])) {
                queue[i] = THREAD_NAME;
                break;
            }
        }
    }
}
