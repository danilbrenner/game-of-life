FROM nginx

ADD nginx.conf /etc/nginx/

EXPOSE 80
COPY ./public /www

CMD ["nginx", "-g", "daemon off;"]