package ru.syudaev.sbertestcase.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import ru.syudaev.sbertestcase.entity.Result;

/**
 * Репозиторий для {@link Result}
 */
public interface ResultRepository extends JpaRepository<Result, Integer> {

}
