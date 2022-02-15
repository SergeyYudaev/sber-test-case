package ru.syudaev.sbertestcase.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;

/**
 * Сущность, обновляемая параллельными потоками.
 */
@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "first_table")
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class)
public class Accumulator {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO, generator = "increment")
    @GenericGenerator(name = "increment", strategy = "increment")
    private Integer key;

    /**
     * Полезная нагрузка.
     * @see ThreadMark
     */
    @Type(type = "jsonb")
    @Column(columnDefinition = "jsonb")
    private ThreadMark value;
}
