package pl.mzlnk.conferencegenerator.generator.entity;

import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.model.entity.order.Buyer;
import pl.mzlnk.conferencegenerator.model.entity.order.Order;
import pl.mzlnk.conferencegenerator.model.entity.order.PaymentType;
import pl.mzlnk.conferencegenerator.repository.data.DataRepositories;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepositories;

import java.util.Calendar;

import static pl.mzlnk.conferencegenerator.utils.RandomUtil.r;

class OrderGenerator extends BaseEntityGenerator {

    private static final int START = 1483232400;
    private static final int END = 1609459199;

    OrderGenerator(EntityRepositories entityRepositories, DataRepositories dataRepositories) {
        super(EntityType.ORDER, entityRepositories, dataRepositories);
    }

    @Override
    public void generate() {
        var orderRepository = entityRepositories.<Order>getRepository(EntityType.ORDER);
        var buyerRepository = entityRepositories.<Buyer>getRepository(EntityType.BUYER);

        buyerRepository
                .findAll()
                .forEach(buyer -> {
                    int orders = r.nextInt(5) + 1;

                    for(int i = 0; i < orders; i++) {
                        int orderId = id++;
                        int buyerId = buyer.getBuyerId();
                        boolean cancelled = (r.nextDouble() < 0.1);
                        PaymentType paymentType = cancelled ? PaymentType.NONE : PaymentType.randomPaymentType();
                        int value = (paymentType.equals(PaymentType.NONE) ? -1 : r.nextInt(10000));

                        Calendar orderDate = Calendar.getInstance();
                        orderDate.setTimeInMillis((r.nextInt(END - START) + START) * 1000L);

                        orderRepository.createOrUpdate(new Order(orderId, buyerId, cancelled, paymentType, value, orderDate));
                    }
                });

        System.out.println("generated: " + orderRepository.findAll().size() + " order entities");
    }

}
